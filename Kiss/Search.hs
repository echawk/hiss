-- | Repository and installed-database lookup (kiss.el's kiss-search).
module Kiss.Search
  ( Recipe (..), pkgFindAll, pkgFindFirst, pkgFind, pkgFindVersion, pkgCache
  ) where

import Control.Monad (filterM, unless)

import Kiss.Env
import Kiss.FS
import Kiss.Glob
import Kiss.Util

-- | A resolved recipe: the reference's @repo_dir@, @repo_name@,
-- @repo_ver@ and @repo_rel@.
data Recipe = Recipe
  { rDir  :: FilePath
  , rName :: String
  , rVer  :: String
  , rRel  :: String
  }

testWith :: String -> FilePath -> IO Bool
testWith t = case t of
  "-x" -> isExec
  "-e" -> exists
  "-f" -> isFile
  _    -> isDir

-- | @_pkg_find QUERY all TEST PATH@: every match, in search order.  As in
-- the reference, the element searched after @PATH@ is @TEST@ itself when a
-- test is given and the installed database otherwise.
pkgFindAll :: Env -> String -> String -> Maybe String -> IO [FilePath]
pkgFindAll e query test mpath = do
  path <- maybe (getVar "KISS_PATH") return mpath
  let final = if null test then eSysDb e else test
      bases = splitColon path ++ [final | not (null final)]
  fmap concat (mapM (\b -> globIn (b ++ "/") query >>= filterM (testWith test)) bases)

-- | @_pkg_find QUERY@ without dying.
pkgFindFirst :: Env -> String -> String -> Maybe String -> IO (Maybe FilePath)
pkgFindFirst e query test mpath = do
  rs <- pkgFindAll e query test mpath
  return (case rs of
    (r : _) -> Just r
    [] -> Nothing)

-- | @pkg_find QUERY@
pkgFind :: Env -> String -> String -> Maybe String -> IO FilePath
pkgFind e query test mpath = do
  r <- pkgFindFirst e query test mpath
  maybe (die e ("'" ++ query ++ "' not found") "") return r

-- | @pkg_find_version PKG "" "" [PATH]@
pkgFindVersion :: Env -> String -> Maybe String -> IO Recipe
pkgFindVersion e name mpath = do
  dir <- pkgFind e name "" mpath
  c <- readFileMaybe (dir ++ "/version")
  let failRead = die e name ("Failed to read version file (" ++ dir ++ "/version)")
  (ver, rel) <- case c of
    -- read(1) fails unless it reaches a newline.
    Just s | '\n' `elem` s -> return (read2 (takeWhile (/= '\n') s))
    _ -> failRead
  unless (not (null rel)) $ die e name "Release field not found in version file"
  x <- isExec (dir ++ "/build")
  unless x $ die e name "Build file not found or not executable"
  return (Recipe dir (afterLast '/' dir) ver rel)

-- | @pkg_cache PKG@: the recipe and its current binary archive, if any.
pkgCache :: Env -> String -> IO (Recipe, Maybe FilePath)
pkgCache e name = do
  r <- pkgFindVersion e name Nothing
  let base = eBinDir e ++ "/" ++ name ++ "@" ++ rVer r ++ "-" ++ rRel r ++ ".tar."
      preferred = base ++ eCompress e
  fallback <- fmap head (globIn base "*")
  p <- isFile preferred
  if p then return (r, Just preferred) else do
    f <- isFile fallback
    return (r, if f then Just fallback else Nothing)
