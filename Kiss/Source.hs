-- | Source resolution, acquisition, verification, and extraction (kiss.el's
-- kiss-source and kiss-download).
module Kiss.Source
  ( Res (..), sourceResolve, pkgSource, checksumGen, pkgVerify, pkgExtract
  , pkgChecksum
  ) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Data.List (isInfixOf, isPrefixOf, sortBy)
import System.Exit (ExitCode (..), exitWith)

import Kiss.Env
import Kiss.File
import Kiss.FS
import Kiss.Glob
import Kiss.Hook
import Kiss.OS
import Kiss.Search
import Kiss.Util

-- | The reference's @_res@ (resolved source) and @_des@ (cache destination).
data Res = Res { resStr :: String, resDes :: String }

-- | @pkg_source_resolve PKG SOURCE DEST [QUIET]@
sourceResolve :: Env -> String -> Recipe -> String -> String -> Bool -> IO Res
sourceResolve e pkg r src dest quiet
  | null src || take 1 src == "#" = return (Res "" "")
  | otherwise = do
      let d = stripTrailing '/' dest
          sub = eSrcDir e ++ "/" ++ pkg ++ "/" ++ (if null d then "" else d ++ "/")
          abs' = "/" ++ dropOneLeading '/' src
      res <-
        if "git+" `isPrefixOf` src then
          return (Res src (beforeLastAny "@#" (sub ++ afterLast '/' src) ++ "/"))
        else if "://" `isInfixOf` src then do
          let cached = sub ++ afterLast '/' src
          f <- isFile cached
          return (if f then Res cached "" else Res ("url+" ++ src) cached)
        else do
          relDir <- isDir (rDir r ++ "/" ++ src)
          absDir <- isDir abs'
          relFile <- isFile (rDir r ++ "/" ++ src)
          absFile <- isFile abs'
          if relDir then return (Res (rDir r ++ "/" ++ src ++ "/.") "")
          else if absDir then return (Res (abs' ++ "/.") "")
          else if relFile then return (Res (rDir r ++ "/" ++ src) "")
          else if absFile then return (Res abs' "")
          else die e pkg ("No local file '" ++ src ++ "'")
      unless quiet $ putStrLn ("found " ++ resStr res)
      return res

-- | @pkg_source PKG [c]@: acquire every source, running source hooks.
pkgSource :: Env -> String -> String -> IO Recipe
pkgSource e pkg mode = do
  r <- pkgFindVersion e pkg Nothing
  let sources = rDir r ++ "/sources"
  has <- isFile sources
  when has $ do
    log2 e pkg "Reading sources"
    c <- fmap (maybe "" id) (readFileMaybe sources)
    forM_ (readLoopOk c) $ \l -> do
      let (src, dest) = read2 l
      res <- sourceResolve e pkg r src dest (not (null mode))
      runHook e ["pre-source", pkg, src]
      let s = resStr res
          fetchUrl = "url+" `isPrefixOf` s
          fetchGit = null mode && "git+" `isPrefixOf` s
      when (fetchUrl || fetchGit) $ do
        mkcd [beforeLast '/' (resDes res)]
        if fetchUrl
          then sourceUrl e r (resDes res) (drop 4 s)
          else sourceGit e r (drop 4 s)
      runHook e ["post-source", pkg, src, if null (resDes res) then s else resDes res]
  return r

-- | @pkg_source_url DEST URL@
sourceUrl :: Env -> Recipe -> FilePath -> String -> IO ()
sourceUrl e r dest url = do
  log2 e (rName r) ("Downloading " ++ url)
  tmp <- tmpFile e (afterLast '/' dest) "download"
  removeIfExists tmp
  let flags = case afterLast '/' (eGet e) of
        "aria2c" -> ["-d", "/", "-o"]
        "axel" -> ["-o"]
        "curl" -> ["-fLo"]
        "wget" -> ["-O"]
        "wget2" -> ["-O"]
        _ -> []
  c <- run (eGet e) (flags ++ [tmp, url])
  unless (c == ExitSuccess) $ do
    removeIfExists tmp
    die e (rName r) ("Failed to download " ++ url)
  run "mv" [tmp, dest] >>= check

-- | @pkg_source_git DEST URL@ in the current (cache) directory.
sourceGit :: Env -> Recipe -> String -> IO ()
sourceGit e r url = do
  let base = beforeLastAny "#@" url
      com = stripPrefixOr base (afterLastAny "@#" url)
  log2 e (rName r) ("Checking out " ++ (if null com then "FETCH_HEAD" else com))
  g <- isDir ".git"
  unless g $ run "git" ["init"] >>= check
  c <- runQuiet "git" ["remote", "set-url", "origin", base]
  unless (c == ExitSuccess) $ run "git" ["remote", "add", "origin", base] >>= check
  run "git" ["fetch", "--depth=1", "origin", com] >>= check
  run "git" ["reset", "--hard", "FETCH_HEAD"] >>= check

-- | @pkg_checksum_gen@: digests of checksum-eligible sources.
checksumGen :: Env -> Recipe -> IO [String]
checksumGen e r = do
  c <- fmap (maybe "" id) (readFileMaybe (rDir r ++ "/sources"))
  files <- fmap concat $ forM (readLoopOk c) $ \l -> do
    let (src, dest) = read2 l
    res <- sourceResolve e (rName r) r src dest True
    let s = resStr res
        s' = if "git+" `isPrefixOf` s then "" else s
    return [s | globMatch "*/*[!.]" s']
  b3raw e files

-- | @pkg_verify PKG@
pkgVerify :: Env -> Recipe -> IO ()
pkgVerify e r = do
  log2 e (rName r) "Verifying sources"
  hs <- checksumGen e r
  unless (null hs) $ do
    mc <- readFileMaybe (rDir r ++ "/checksums")
    c <- maybe (exitWith (ExitFailure 1)) return mc
    let (complete, partial) = splitRecords c
    loop complete partial hs
  where
    loop (l : ls) partial hs = body (fst (read2 l)) hs >>= loop ls partial
    loop [] partial hs
      | null hs = return ()
      | otherwise = body (maybe "" (fst . read2) partial) hs >>= loop [] Nothing
    body chk hs = do
      when (length chk == 64) $ do
        logK e (rName r) "Detected sha256 checksums." "ERROR"
        log1 e "blake3 is the new checksum provider for kiss. Please run"
        log1 e ("'kiss checksum " ++ rName r ++ "' to regenerate the checksums file.")
        exitWith (ExitFailure 1)
      let h = case hs of
            (x : _) -> x
            [] -> ""
          dchk = if null chk then "null" else chk
      putStrLn ("- " ++ (if null chk then "missing" else chk))
      putStrLn ("+ " ++ (if null h then "no source" else h))
      unless (h ++ "-" ++ dchk == chk ++ "-" ++ h || h ++ "-" ++ dchk == h ++ "-SKIP") $
        die e (rName r) "Checksum mismatch"
      return (drop 1 hs)

splitRecords :: String -> ([String], Maybe String)
splitRecords s = (readLoop s, case reverse s of
  [] -> Nothing
  ('\n' : _) -> Nothing
  _ -> Just (reverse (takeWhile (/= '\n') (reverse s))))

-- | @pkg_extract PKG@
pkgExtract :: Env -> Recipe -> IO ()
pkgExtract e r = do
  let pkg = rName r
  log2 e pkg "Extracting sources"
  runHook e ["pre-extract", pkg, ePkgDir e ++ "/" ++ pkg]
  c <- fmap (maybe "" id) (readFileMaybe (rDir r ++ "/sources"))
  (status, lastRes) <- foldM (\_ l -> do
      let (src, dest) = read2 l
      res <- sourceResolve e pkg r src dest True
      let s = resStr res
      unless (null s) $ mkdirCd (eMakDir e ++ "/" ++ pkg ++ "/" ++ dest)
      st <- if "git+" `isPrefixOf` s then run "cp" ["-PRf", resDes res ++ "/.", "."]
            else if any (`globMatch` s) archives then sourceTar e r s
            else if not (null s) then run "cp" ["-PRf", s, "."]
            else return ExitSuccess
      return (st, s)) (ExitSuccess, "") (readLoopOk c)
  unless (status == ExitSuccess) $ die e pkg ("Failed to extract " ++ lastRes)
  where
    archives = ["*.tar", "*.tar.??", "*.tar.???", "*.tar.????", "*.t?z"]
    -- @mkcd@ with @set -e@ suspended: failure leaves the directory unchanged.
    mkdirCd d = do
      c <- run "mkdir" ["-p", d]
      when (c == ExitSuccess) (cd d)

-- | @pkg_source_tar ARCHIVE@: extract into the current directory, moving
-- the contents of every top-level directory up one level.
sourceTar :: Env -> Recipe -> FilePath -> IO ExitCode
sourceTar e r file = do
  let name = rName r
  tarball <- tmpFile e name "tarball"
  listing <- tmpFile e name "tarball-manifest"
  c1 <- decompressTo file tarball
  unless (c1 == ExitSuccess) $ die e name ("Failed to decompress " ++ file)
  c2 <- run "tar" ["xf", tarball]
  unless (c2 == ExitSuccess) $ die e name ("Failed to extract " ++ file)
  -- The reference checks only the status of sort in "tar tf | sort".
  (_, out) <- capture defSpec "tar" ["tf", tarball]
  let tops = uniqBy (sortBy (\a b -> compare (key a) (key b)) (lines out))
      key = takeWhile (/= '/')
  writeFileBin listing (concatMap (++ "\n") tops)
  forM_ tops $ \l -> do
    let dir = key l
    unless (null (dropOneLeading '.' dir)) $ do
      d <- isDir dir
      when d $ do
        let moved = ePid e ++ "-" ++ dir
        _ <- run "mv" ["-f", dir, moved]
        c <- withDevNull (\n -> spawn defSpec { sErr = UseHandle n } "find"
               [moved ++ "/.", "!", "-name", ".", "-prune", "-exec", "sh", "-c"
               , "mv -f \"$0\" \"$@\" .", "{}", "+"])
        unless (c == ExitSuccess) $ do
          _ <- run "find" [moved ++ "/.", "!", "-name", ".", "-prune", "-exec", "sh", "-c"
                          , "cp -fRPp \"$0\" \"$@\" .", "{}", "+"]
          return ()
        _ <- run "rm" ["-rf", moved]
        return ()
  removeIfExists tarball
  return ExitSuccess
  where
    uniqBy (a : b : rest) | takeWhile (/= '/') a == takeWhile (/= '/') b = uniqBy (a : rest)
    uniqBy (a : rest) = a : uniqBy rest
    uniqBy [] = []

-- | @pkg_checksum PKG@
pkgChecksum :: Env -> String -> IO ()
pkgChecksum e pkg = do
  r <- pkgSource e pkg "c"
  has <- isFile (rDir r ++ "/sources")
  when has $ do
    hs <- checksumGen e r
    unless (null hs) $ putStr (concatMap (++ "\n") hs)
    if null hs
      then log2 e pkg "No sources needing checksums"
      else do
        writeFileBin (rDir r ++ "/checksums") (concatMap (++ "\n") hs)
        log2 e pkg "Generated checksums"
