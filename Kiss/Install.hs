-- | Binary package installation and removal (kiss.el's kiss-install and
-- kiss-remove).
module Kiss.Install (pkgInstall, pkgRemove) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef
import Data.List (intercalate, isInfixOf, isPrefixOf, sort)
import System.Directory (canonicalizePath, removeDirectory, removeFile)
import System.Exit (ExitCode (..), exitWith)
import System.IO

import Kiss.Alternatives
import Kiss.Env
import Kiss.File
import Kiss.FS
import Kiss.Glob
import Kiss.Hook
import Kiss.Manifest
import Kiss.OS
import Kiss.Search
import Kiss.Util

-- | @resolve_path PATH@: physically canonicalize an existing parent.
resolvePath :: Env -> String -> IO String
resolvePath e p = do
  let r = eRoot e ++ "/" ++ dropOneLeading '/' p
      parent = beforeLast '/' r
  d <- if null parent then return False else isDir parent
  par <- if d then fmap (either (const parent) id) (tryIO (canonicalizePath parent))
              else return parent
  return (stripPrefixOr (eRoot e) par ++ "/" ++ afterLast '/' r)

-- | @pkg_install PKG|ARCHIVE@
pkgInstall :: Env -> String -> IO ()
pkgInstall e arg = do
  (tarFile, pkg) <-
    if globMatch "*.tar.*" arg
      then do
        f <- isFile arg
        unless f $ die e ("File '" ++ arg ++ "' does not exist") ""
        return (arg, beforeLast '@' (afterLast '/' arg))
      else do
        (_, mt) <- pkgCache e arg
        t <- maybe (die e arg "Not yet built") return mt
        return (t, arg)
  trapOff e
  mkcd [eTarDir e ++ "/" ++ pkg]
  extractArchive tarFile >>= check
  pwd <- getPWD
  let meta = pwd ++ "/" ++ pkgDb ++ "/" ++ pkg
  hasMan <- isFile (meta ++ "/manifest")
  unless hasMan $ die e "Not a valid KISS package" ""
  force <- getVar "KISS_FORCE"
  unless (force == "1") $ do
    manifestValidate e pkg
    pkgInstallable e pkg (meta ++ "/depends")
  runHook e ["pre-install", pkg, pwd]
  pkgConflicts e pkg
  log2 e pkg ("Installing package (" ++ afterLast '/' tarFile ++ ")")
  let sys = eSysDb e ++ "/" ++ pkg
  oldMan <- tmpFileCopy e pkg "manifest-copy" (sys ++ "/manifest")
  oldEtc <- tmpFileCopy e pkg "etcsums-copy" (sys ++ "/etcsums")
  _ <- tmpFile e pkg "manifest-diff"
  newMan <- fmap (maybe "" id) (readFileMaybe (meta ++ "/manifest"))
  oldLines <- fmap (maybe [] grepLines) (readFileMaybe oldMan)
  let newSet = mkSet (grepLines newMan)
      oldOnly = [l | l <- oldLines, not (l `member` newSet)]
      sorted = sort (grepLines newMan)
  _ <- tmpFile e pkg "manifest-reverse"
  etcs <- fmap (maybe [] etcStream) (readFileMaybe oldEtc)
  ok1 <- installFiles e pkg False pwd sorted etcs
  ok2 <- if ok1 then removeFiles e oldOnly etcs else return False
  ok3 <- if ok2 then installFiles e pkg True pwd sorted etcs else return False
  if ok3
    then do
      trapOn e
      runHookPkg e "post-install" pkg
      runHook e ["post-install", pkg, sys]
      log2 e pkg "Installed successfully"
    else do
      pkgClean e
      logK e pkg "Failed to install package." "ERROR"
      die e pkg "Filesystem now dirty, manual repair needed."

-- | Records read from file descriptor 3 by repeated @read -r sum@.
etcStream :: String -> [String]
etcStream = map read1 . lines

pop :: [String] -> (String, [String])
pop (x : xs) = (x, xs)
pop [] = ("", [])

-- | @pkg_manifest_validate@
manifestValidate :: Env -> String -> IO ()
manifestValidate e pkg = do
  log2 e pkg "Checking if manifest valid"
  pwd <- getPWD
  c <- fmap (maybe "" id) (readFileMaybe (pwd ++ "/" ++ pkgDb ++ "/" ++ pkg ++ "/manifest"))
  missing <- fmap concat $ forM (map read1 (readLoop c)) $ \l -> do
    x <- lexists (eTarDir e ++ "/" ++ pkg ++ l)
    if x then return [] else putStrLn l >> return [l]
  unless (null missing) $
    die e pkg ("manifest contains " ++ show (length missing) ++ " non-existent files")

-- | @pkg_installable PKG DEPENDS@
pkgInstallable :: Env -> String -> FilePath -> IO ()
pkgInstallable e pkg depends = do
  log2 e pkg "Checking if package installable"
  f <- isFile depends
  when f $ do
    c <- fmap (maybe "" id) (readFileMaybe depends)
    missing <- fmap concat $ forM (readLoopOk c) $ \l -> do
      let (dep, ty) = read2 l
      if globMatch "[!#]?* " (dep ++ " " ++ ty)
        then do
          d <- isDir (eSysDb e ++ "/" ++ dep)
          if d then return [] else putStrLn (dep ++ " " ++ ty) >> return [dep]
        else return []
    unless (null missing) $
      die e pkg ("Package not installable, missing " ++ show (length missing) ++ " package(s)")

-- | @pkg_conflicts@
pkgConflicts :: Env -> String -> IO ()
pkgConflicts e pkg = do
  log2 e pkg "Checking for package conflicts"
  _ <- tmpFile e pkg "manifest-files"
  _ <- tmpFile e pkg "found-conflicts"
  pwd <- getPWD
  man <- fmap (maybe "" id) (readFileMaybe (pwd ++ "/" ++ pkgDb ++ "/" ++ pkg ++ "/manifest"))
  resolved <- mapM (resolvePath e) [f | f <- map read1 (readLoop man), globMatch "*[!/]" f]
  cd (eTarDir e ++ "/" ++ pkg)
  manifests <- globIn (eSysDb e ++ "/") "*/manifest"
  let others = [m | m <- manifests, m /= eSysDb e ++ "/" ++ pkg ++ "/manifest"]
  unless (null others) $ do
    let pats = mkSet resolved
        prefix = length others > 1
    found <- fmap concat $ forM others $ \m -> do
      c <- readFileMaybe m
      return [if prefix then m ++ ":" ++ l else l
             | Just s <- [c], not (null resolved), l <- grepLines s, l `member` pats]
    if any (":/var/db/kiss/installed/" `isInfixOf`) found
      then return ()
      else writeIORef (eSafe e) True
    safe <- readIORef (eSafe e)
    choice <- getVar "KISS_CHOICE"
    if choice /= "1" && safe && not (null found)
      then do
        here <- getPWD
        forM_ found $ \line -> do
          let con = if ':' `elem` line then afterFirst ':' line else ""
          putStrLn ("Found conflict " ++ con)
          mkcd' (here ++ "/" ++ choDb)
          let enc = replaceAll "/" ">" con
          c <- runQuiet "mv" ["-f", here ++ con, here ++ "/" ++ choDb ++ "/" ++ pkg ++ enc]
          unless (c == ExitSuccess) $ do
            log1 e ("File must be in " ++ beforeLast '/' con ++ " and not a symlink to it")
            log1 e "This usually occurs when a binary is installed to"
            die e "/sbin instead of /usr/bin (example)" ""
        log2 e pkg "Converted all conflicts to choices (kiss a)"
        pkgManifest e pkg (eTarDir e)
      else when (not (null found)) $ do
        logK e ("Package '" ++ pkg ++ "' conflicts with another package") "" "!>"
        logK e ("Run 'KISS_CHOICE=1 kiss i " ++ pkg ++ "' to add conflicts") "" "!>"
        logK e "as alternatives." "" "!>"
        exitWith (ExitFailure 1)
  where
    mkcd' d = run "mkdir" ["-p", d] >>= check

-- | @pkg_install_files -z|-e SRC@ over sorted manifest records.  Returns
-- False when a copy or directory creation fails.
installFiles :: Env -> String -> Bool -> FilePath -> [String] -> [String] -> IO Bool
installFiles e pkg verify src files etcs0 = go (map read1 files) etcs0
  where
    root = eRoot e
    go [] _ = return True
    go (file : rest) etcs
      | "/" `isSuffixOfStr` file = do
          let target = root ++ file
          d <- isDir target
          ok <- if d then return True else do
            oct <- fileRwx (src ++ "/" ++ dropOneLeading '/' file)
            fmap (== ExitSuccess) (run "mkdir" ["-m", oct, target])
          if ok then go rest etcs else return False
      | otherwise = do
          let target = root ++ file
          d <- isDir target
          skip <- if d || not verify then return False else exists target
          if skip then go rest etcs else
            if globMatch "/etc/*[!/]" file
              then do
                (install, target', etcs') <- pkgEtc e pkg file target etcs
                if install then copyOne file target' >>= next rest etcs'
                           else go rest etcs'
              else copyOne file target >>= next rest etcs
    next rest etcs ok = if ok then go rest etcs else return False

    copyOne file target = do
      l <- isLink target
      if l
        then fmap (== ExitSuccess) (run "cp" ["-fP", src ++ file, beforeLast '/' target ++ "/."])
        else do
          let tmp = beforeLast '/' target ++ "/__kiss-tmp-" ++ pkg ++ "-"
                    ++ afterLast '/' file ++ "-" ++ ePid e
          c1 <- run "cp" ["-fP", src ++ file, tmp]
          if c1 /= ExitSuccess then return False else do
            sl <- isLink (src ++ file)
            c2 <- if sl then return ExitSuccess else run "touch" ["-r", src ++ file, tmp]
            if c2 /= ExitSuccess then return False else
              fmap (== ExitSuccess) (run "mv" ["-f", tmp, target])

isSuffixOfStr :: String -> String -> Bool
isSuffixOfStr s x = reverse s `isPrefixOf` reverse x

-- | @pkg_etc@: the /etc three-way checksum decision.  Returns whether to
-- install, the destination (possibly @PATH.new@), and the remaining stream.
pkgEtc :: Env -> String -> String -> FilePath -> [String] -> IO (Bool, FilePath, [String])
pkgEtc e pkg file target etcs = do
  let (old, rest) = pop etcs
      args = [eTarDir e ++ "/" ++ pkg ++ file, eRoot e ++ file]
  hs <- (if length old == 64 then sh256 else b3) e args
  let hash = intercalate "\n" hs
      new = takeWhile (/= '\n') hash
      sys = afterFirst '\n' hash
      dflt x = if null x then "null" else x
      subject = dflt old ++ dflt sys ++ new
  if subject == new ++ sys ++ old
    then return (False, target, rest)
    else if subject == old ++ old ++ old
            || subject == dflt old ++ sys ++ sys
            || (sys ++ old) `isPrefixOf` subject
      then return (True, target, rest)
      else do
        war e pkg ("saving " ++ file ++ " as " ++ file ++ ".new")
        return (True, target ++ ".new", rest)

-- | @pkg_remove_files@ with checksum records on file descriptor 3.
removeFiles :: Env -> [String] -> [String] -> IO Bool
removeFiles e files etcs0 = go (map read1 files) etcs0 []
  where
    root = eRoot e
    go [] _ syms = do
      rs <- forM syms $ \s -> do
        x <- exists s
        if x then return True else rmF s
      return (null rs || last rs)
    go (file : rest) etcs syms
      | globMatch "/etc/?*[!/]" file = do
          let (want, etcs') = pop etcs
          hs <- (if length want == 64 then sh256 else b3) e [root ++ "/" ++ file]
          if intercalate "\n" hs /= want
            then do
              putStrLn ("Skipping " ++ file ++ " (modified)")
              go rest etcs' syms
            else remove file rest etcs' syms
      | otherwise = remove file rest etcs syms
    remove file rest etcs syms = do
      let f = (if null root then "" else root ++ "/") ++ dropOneTrailing '/' file
      l <- isLink f
      d <- isDir f
      if l && d
        then go rest etcs (if globMatch "/*/*/" file then syms ++ [f] else syms)
        else do
          if d then tryIO (removeDirectory f) >> return () else rmF f >> return ()
          go rest etcs syms
    rmF f = do
      r <- tryIO (removeFile f)
      case r of
        Right () -> return True
        Left _ -> fmap not (lexists f)

-- | @pkg_removable PKG@
pkgRemovable :: Env -> String -> IO ()
pkgRemovable e pkg = do
  log2 e pkg "Checking if package removable"
  old <- getPWD
  cd (eSysDb e)
  dependFiles <- glob "*/depends"
  hits <- fmap concat $ forM dependFiles $ \f -> do
    c <- readFileMaybe f
    case c of
      Nothing -> do
        hPutStrLn stderr ("grep: " ++ f ++ ": No such file or directory")
        return []
      Just s -> return [f | pkg `elem` grepLines s]
  mapM_ putStrLn hits
  unless (null hits) $ die e pkg "Not removable, has dependents"
  cd old
  _ <- tmpFile e pkg "owned-conflicts"
  -- pkg_preferred runs in a pipeline here, so its cd does not leak.
  recs <- pkgPreferred e [pkg]
  unless (null recs) $
    die e pkg ("Not removable, package leaves behind " ++ show (length recs) ++ " orphaned alternatives")

-- | @pkg_remove PKG@
pkgRemove :: Env -> String -> IO ()
pkgRemove e pkg = do
  let sys = eSysDb e ++ "/" ++ pkg
  d <- isDir sys
  unless d $ die e ("'" ++ pkg ++ "' not installed") ""
  trapOff e
  force <- getVar "KISS_FORCE"
  unless (force == "1") $ pkgRemovable e pkg
  runHookPkg e "pre-remove" pkg
  runHook e ["pre-remove", pkg, sys]
  etc <- tmpFileCopy e pkg "etcsums-copy" (sys ++ "/etcsums")
  log2 e pkg "Removing package"
  man <- readFileMaybe (sys ++ "/manifest")
  case man of
    Nothing -> do
      hPutStrLn stderr ("cannot open " ++ sys ++ "/manifest")
      exitWith (ExitFailure 1)
    Just m -> do
      etcs <- fmap (maybe [] etcStream) (readFileMaybe etc)
      _ <- removeFiles e (readLoop m) etcs
      return ()
  trapOn e
  log2 e pkg "Removed successfully"
