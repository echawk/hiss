-- | Package construction and system upgrade (kiss.el's kiss-build).
module Kiss.Build (pkgBuildAll, pkgUpgrade) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, sortBy)
import Foreign.Marshal.Alloc (allocaBytes)
import System.Exit (ExitCode (..), exitWith)
import System.IO
import System.Process
import System.Directory (canonicalizePath)

import Kiss.Env
import Kiss.File
import Kiss.FS
import Kiss.Glob
import Kiss.Hook
import Kiss.Manifest
import Kiss.OS
import Kiss.Package
import Kiss.Search
import Kiss.Source
import Kiss.Util

-- | @(export KISS_FORCE=1; args i ARGS...)@
installSubshell :: Env -> [String] -> IO ()
installSubshell e args = do
  c <- subshell e (setVar "KISS_FORCE" "1" >> eDispatch e ("i" : args))
  check c

-- | @pkg_build_all PKG...@
pkgBuildAll :: Env -> [String] -> IO ()
pkgBuildAll e pkgs = do
  force <- getVar "KISS_FORCE"
  depsRef <- newIORef []
  explicitRef <- newIORef []
  forM_ pkgs $ \p -> do
    ex <- readIORef explicitRef
    unless (force == "1") $ pkgDepends e depsRef ex p "expl" "filter" "" ""
    modifyIORef explicitRef (++ [p])
  explicitAll <- readIORef explicitRef
  preferCache <- readIORef (ePreferCache e)
  let explicitBuild = if null preferCache then explicitAll else []
  deps <- readIORef depsRef
  let explicit = [p | p <- explicitAll, p `notElem` deps]
  log1 e ("Building: explicit: " ++ unwords explicit
          ++ (if null deps then "" else ", implicit: " ++ unwords deps))
  let queue0 = deps ++ explicit
  when (length queue0 /= length explicit) $ prompt e ""
  log1 e "Checking for pre-built dependencies"
  queue <- filterM (\p ->
    if p `elem` explicitBuild then return True else do
      (_, cached) <- pkgCache e p
      case cached of
        Just tarball -> do
          log2 e p "Found pre-built binary"
          installSubshell e [tarball]
          return False
        Nothing -> return True) queue0
  forM_ queue $ \p -> do
    r <- pkgSource e p ""
    has <- isFile (rDir r ++ "/sources")
    when has $ pkgVerify e r
  let total = length queue
  forM_ queue $ \p -> do
    modifyIORef (eBuildCur e) (+ 1)
    cur <- readIORef (eBuildCur e)
    log2 e p ("Building package (" ++ show cur ++ "/" ++ show total ++ ")")
    r <- pkgFindVersion e p Nothing
    runHook e ["queue-status", p, show cur, show total]
    has <- isFile (rDir r ++ "/sources")
    when has $ pkgExtract e r
    pkgBuild e r
    pkgManifest e p (ePkgDir e)
    pkgStrip e p
    cd (ePkgDir e ++ "/" ++ p ++ "/" ++ pkgDb ++ "/" ++ p)
    pkgFixDeps e r
    pkgEtcsums e r
    pkgTar e r
    pc <- readIORef (ePreferCache e)
    let pc' = if null pc then "0" else pc
    writeIORef (ePreferCache e) pc'
    when (pc' == "1" || p `notElem` explicit) $ do
      log2 e p "Needed as a dependency or has an update, installing"
      installSubshell e [p]
  bi <- readIORef (eBuildInstall e)
  let bi' = if null bi then "1" else bi
  writeIORef (eBuildInstall e) bi'
  kp <- getVarOr "KISS_PROMPT" "1"
  when (bi' == "1" && kp == "1") $ do
    prompt e ("Install built packages? [" ++ unwords explicit ++ "]")
    c <- subshell e (eDispatch e ("i" : explicit))
    check c

-- | @pkg_build PKG@: run the build script with output teed to a log.
pkgBuild :: Env -> Recipe -> IO ()
pkgBuild e r = do
  let p = rName r
      bdir = eMakDir e ++ "/" ++ p
      logf = eLogDir e ++ "/" ++ p ++ "-" ++ eTime e ++ "-" ++ ePid e
  mkcd [bdir, ePkgDir e ++ "/" ++ p ++ "/" ++ pkgDb]
  log2 e p "Starting build"
  runHook e ["pre-build", p, bdir]
  lr <- tryIO (writeFileBin logf "")
  case lr of
    Left _ -> do
      hPutStrLn stderr ("cannot create " ++ logf)
      exitWith (ExitFailure 1)
    Right () -> return ()
  pwd <- getPWD
  let dflt k d = do
        v <- getVar k
        return (k, if null v then d else v)
      after k pre = do
        v <- getVar k
        return (k, pre ++ v)
  over <- sequence
    [ dflt "AR" "ar", dflt "CC" "cc", dflt "CXX" "c++", dflt "NM" "nm"
    , dflt "RANLIB" "ranlib"
    , after "RUSTFLAGS" ("--remap-path-prefix=" ++ pwd ++ "=. ")
    , after "GOFLAGS" "-trimpath -modcacherw "
    , return ("GOPATH", pwd ++ "/go")
    , return ("KISS_ROOT", eRoot e) ]
  env' <- currentEnvWith over
  code <- runTee env' (rDir r ++ "/build") [ePkgDir e ++ "/" ++ p, rVer r] logf
  unless (code == ExitSuccess) $ do
    log2 e p "Build failed"
    log2 e p ("Log stored to " ++ logf)
    _ <- subshell e (runHook e ["build-fail", p, bdir])
    pkgClean e
    -- The reference ends here with `kill 0`, terminating the process group
    -- without running its EXIT trap.
    trapOff e
    exitWith (ExitFailure 1)
  keep <- getVar "KISS_KEEPLOG"
  unless (keep == "1") $ removeIfExists logf
  run "cp" ["-LRf", rDir r, ePkgDir e ++ "/" ++ p ++ "/" ++ pkgDb ++ "/"] >>= check
  log2 e p "Successfully built package"
  runHook e ["post-build", p, ePkgDir e ++ "/" ++ p]

-- | Run a command with standard output and error combined, copying the
-- stream to our standard output and to a log file (@2>&1 | tee LOG@).
runTee :: [(String, String)] -> FilePath -> [String] -> FilePath -> IO ExitCode
runTee env' cmd args logf = do
  hFlush stdout
  hFlush stderr
  (rd, wr) <- createPipe
  r <- tryIO (createProcess (proc cmd args)
         { env = Just env', std_out = UseHandle wr, std_err = UseHandle wr })
  case r of
    Left err -> do
      hClose rd
      hPutStrLn stderr (cmd ++ ": " ++ show err)
      return (ExitFailure 127)
    Right (_, _, _, ph) -> do
      hSetBinaryMode rd True
      withBinaryFile logf AppendMode $ \lh ->
        allocaBytes 4096 $ \buf ->
          let loop = do
                n <- hGetBufSome rd buf 4096
                when (n > 0) $ do
                  hPutBuf stdout buf n
                  hFlush stdout
                  hPutBuf lh buf n
                  loop
          in loop
      hClose rd
      waitForProcess ph

-- | @pkg_strip PKG@
pkgStrip :: Env -> String -> IO ()
pkgStrip e p = do
  nostrip <- isFile (eMakDir e ++ "/" ++ p ++ "/nostrip")
  ks <- getVar "KISS_STRIP"
  unless (nostrip || ks == "0") $ do
    log2 e p "Stripping binaries and libraries"
    c <- fmap (maybe "" id) (readFileMaybe (ePkgDir e ++ "/" ++ p ++ "/" ++ pkgDb ++ "/" ++ p ++ "/manifest"))
    forM_ (map read1 (readLoop c)) $ \f -> do
      let path = ePkgDir e ++ "/" ++ p ++ f
      l <- isLink path
      when (not l && isStripCandidate f) $ do
        od <- fmap odRender (readHead 18 path)
        let rel = globMatch "*177*E*L*F*0000020 001 *" od || globMatch "*!*<*a*r*c*h*>*" od
            dyn = globMatch "*177*E*L*F*0000020 00[23] *" od
            strip flags = do
              let args = flags ++ ["-R", ".comment", "-R", ".note", path]
              putStrLn (unwords ("strip" : args))
              _ <- run "strip" args
              return ()
        if rel then strip ["-g"] else when dyn (strip ["-s"])

-- | @should_ignore_rpath VALUE@
shouldIgnoreRpath :: String -> Bool
shouldIgnoreRpath v = any absolute (splitColonFields v)
  where
    absolute pth = not ("$ORIGIN" `isPrefixOf` pth || "${ORIGIN}" `isPrefixOf` pth)
    splitColonFields s = if null s then [] else dropLastEmpty (splitOn ':' s)
    dropLastEmpty xs = if not (null xs) && null (last xs) then init xs else xs

-- | @pkg_fix_deps PKG@ in the staged database directory.
pkgFixDeps :: Env -> Recipe -> IO ()
pkgFixDeps e r = do
  let p = rName r
      elfTool = eElf e
  log2 e p ("looking for dependencies (using " ++ afterLast '/' elfTool ++ ")")
  pre <- tmpFileCopy e p "depends" "depends"
  fixed <- tmpFile e p "depends-fixed"
  manifests <- globIn (eSysDb e ++ "/") "*/manifest"
  man <- fmap (maybe "" id) (readFileMaybe "manifest")
  seenRef <- newIORef []
  owners <- fmap concat $ forM (map read1 (readLoop man)) $ \f -> do
    -- The reference tests the manifest path on the host, not in the stage.
    l <- isLink f
    if l || not (isStripCandidate f) then return [] else do
      let path = ePkgDir e ++ "/" ++ p ++ f
      elf <- if "readelf" `isSuffixOf` elfTool
               then fmap snd (withDevNull (\n -> capture defSpec { sErr = UseHandle n } elfTool ["-d", path]))
               else return ""
      if rpathIgnored elf then return [] else do
        (lc, ldd) <- withDevNull (\n -> capture defSpec { sErr = UseHandle n } "ldd" ["--", path])
        if lc /= ExitSuccess then return [] else do
          let info = if null elf then ldd else elf
          fmap concat $ forM (lines info) $ \line -> do
            let ws = words line
                entry = if length ws > 1 then ws !! 1 else ""
                rest0 = trimWs (dropField (dropField line))
            if entry /= "(NEEDED)" && entry /= "=>" then return [] else do
              let lib0 = beforeFirstStr "]" (afterLastStr "[" rest0)
                  lib1 = if "readelf" `isSuffixOf` elfTool
                           then stripPrefixOr (ePkgDir e ++ "/" ++ p) (afterFirstStr ("\t" ++ lib0 ++ " => ") ldd)
                           else lib0
                  lib = takeWhile (/= ' ') lib1
              if systemLib (afterLast '/' lib) then return [] else do
                seen <- readIORef seenRef
                if lib `elem` seen then return [] else do
                  writeIORef seenRef (seen ++ [lib])
                  rp <- resolveLib lib
                  self <- grepFiles rp ["manifest"]
                  if not (null self) then return [] else do
                    o <- pkgOwner e rp (Just manifests)
                    return (maybe [] (: []) o)
  old <- fmap (maybe "" id) (readFileMaybe pre)
  let merged = uniqKey (sortBy (\a b -> compare (key a) (key b)) (lines old ++ owners))
  writeFileBin fixed (concatMap (++ "\n") merged)
  nonEmpty <- isNonEmpty fixed
  when nonEmpty $ do
    withDevNull (\n -> spawn defSpec { sErr = UseHandle n } "diff" ["-U", "3", pre, fixed]) >> return ()
    run "mv" ["-f", fixed, "depends"] >>= check
    pwd <- getPWD
    pkgManifest e (afterLast '/' pwd) (ePkgDir e)
  where
    dropField s = dropWhile (not . isIfsWs) (dropWhile isIfsWs s)
    key l = let (ws, rest) = span isIfsWs l in ws ++ takeWhile (not . isIfsWs) rest
    uniqKey (a : b : rest) | key a == key b = uniqKey (a : rest)
    uniqKey (a : rest) = a : uniqKey rest
    uniqKey [] = []
    rpathIgnored elf = go (lines elf)
      where
        go [] = False
        go (l : ls) =
          let ws = words l
              entry = if length ws > 1 then ws !! 1 else ""
              value = beforeFirstStr "]" (afterLastStr "[" (trimWs (dropField (dropField l))))
          in if entry == "(RPATH)" || entry == "(RUNPATH)"
               then shouldIgnoreRpath value
               else go ls
    resolveLib lib = do
      let rpath = eRoot e ++ "/" ++ dropOneLeading '/' lib
          parent = beforeLast '/' rpath
      d <- if null parent then return False else isDir parent
      par <- if d then fmap (either (const parent) id) (tryIO (canonicalizePath parent))
             else return parent
      return (stripPrefixOr (eRoot e) par ++ "/" ++ afterLast '/' rpath)

systemLib :: String -> Bool
systemLib b = any (`globMatch` b)
  [ "ld-*", "lib[cm].so*", "libc++.so*", "libc++abi.so*", "libcrypt.so*"
  , "libdl.so*", "libgcc_s.so*", "libmvec.so*", "libpthread.so*"
  , "libresolv.so*", "librt.so*", "libstdc++.so*", "libtrace.so*"
  , "libunwind.so*", "libutil.so*", "libxnet.so*", "ldd" ]

-- | @${s##*X}@ and @${s%%X*}@ / @${s#*X}@ for string separators.
afterLastStr, beforeFirstStr, afterFirstStr :: String -> String -> String
afterLastStr sep s = go s s
  where
    go acc [] = acc
    go acc r@(_ : rs) = if sep `isPrefixOf` r then go (drop (length sep) r) rs else go acc rs
beforeFirstStr sep s = go s
  where
    go [] = []
    go r@(c : rs) = if sep `isPrefixOf` r then [] else c : go rs
afterFirstStr sep s = maybe s id (findSub sep s)

-- | @pkg_etcsums@
pkgEtcsums :: Env -> Recipe -> IO ()
pkgEtcsums e r = do
  let p = rName r
  log2 e p "Generating etcsums"
  hasEtc <- isDir (ePkgDir e ++ "/" ++ p ++ "/etc")
  when hasEtc $ do
    man <- fmap (maybe "" id) (readFileMaybe "manifest")
    files <- forM [f | f <- map read1 (readLoop man), globMatch "/etc/*[!/]" f] $ \f -> do
      let path = ePkgDir e ++ "/" ++ p ++ "/" ++ f
      l <- isLink path
      return (if l then "/dev/null" else path)
    hs <- b3 e files
    writeFileBin "etcsums" (concatMap (++ "\n") hs)

-- | @pkg_tar PKG@
pkgTar :: Env -> Recipe -> IO ()
pkgTar e r = do
  let p = rName r
      tarball = eBinDir e ++ "/" ++ p ++ "@" ++ rVer r ++ "-" ++ rRel r ++ ".tar." ++ eCompress e
  log2 e p "Creating tarball"
  old <- getPWD
  cd (ePkgDir e ++ "/" ++ p)
  createArchive (eCompress e) tarball >>= check
  cd old
  log2 e p "Successfully created tarball"
  runHook e ["post-package", p, tarball]

-- | @pkg_upgrade@
pkgUpgrade :: Env -> IO ()
pkgUpgrade e = do
  log1 e "Checking for new package versions"
  installed <- globIn (eSysDb e ++ "/") "*"
  results <- forM installed $ \path -> do
    let name = afterLast '/' path
    old <- pkgFindVersion e name (Just (eSysDb e))
    new <- pkgFindVersion e name Nothing
    let orphan = globMatch "*/var/db/kiss/installed/*" (rDir new)
        ov = rVer old ++ "-" ++ rRel old
        nv = rVer new ++ "-" ++ rRel new
        changed = ov /= nv
    when changed $ putStrLn (name ++ " " ++ ov ++ " => " ++ nv)
    return (name, orphan, changed)
  let orphans = [n | (n, True, _) <- results]
      changed = [n | (n, _, True) <- results]
  unless (null orphans) $ war e ("Packages without repository" ++ concatMap ('\n' :) orphans) ""
  writeIORef (eBuildInstall e) "0"
  writeIORef (ePreferCache e) "1"
  if "kiss" `elem` changed
    then do
      log1 e "Detected package manager update"
      log1 e "The package manager will be updated first"
      prompt e ""
      pkgBuildAll e ["kiss"]
      log1 e "Updated the package manager"
      log1 e "Re-run 'kiss update' to update your system"
    else if null changed
      then log1 e "Nothing to do"
      else do
        (order, _) <- pkgOrder e changed
        prompt e ("Packages to update (" ++ show (length order) ++ "): " ++ unwords order)
        pkgBuildAll e order
        log1 e "Updated all packages"
