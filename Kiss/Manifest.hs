-- | Manifest generation, rewriting, and ownership queries (kiss.el's
-- kiss-manifest).
module Kiss.Manifest
  ( pkgManifest, pkgManifestReplace, pkgOwner, grepFiles, findTree
  ) where

import Control.Monad (forM)
import Data.List (sort, isSuffixOf)

import Kiss.Env
import Kiss.FS
import Kiss.Glob
import Kiss.OS
import Kiss.Util

-- | @find TOP ! -path TOP@ without following symlinks: each entry and
-- whether it is a directory, in traversal order.
findTree :: FilePath -> IO [(FilePath, Bool)]
findTree top = do
  names <- getEntries top
  fmap concat $ forM [n | n <- names, n /= ".", n /= ".."] $ \n -> do
    let p = top ++ "/" ++ n
    l <- isLink p
    d <- if l then return False else isDir p
    if d then fmap ((p, True) :) (findTree p) else return [(p, False)]

-- | @pkg_manifest PKG DIR@
pkgManifest :: Env -> String -> FilePath -> IO ()
pkgManifest e name dir = do
  log2 e name "Generating manifest"
  _ <- tmpFile e name "manifest"
  let top = dir ++ "/" ++ name
      dbdir = top ++ "/" ++ pkgDb ++ "/" ++ name
  etc <- isDir (top ++ "/etc")
  tree <- findTree top
  let excluded p = let b = afterLast '/' p in ".la" `isSuffixOf` b || b == "charset.alias"
      found = [if d then p ++ "/" else p | (p, d) <- tree, d || not (excluded p)]
      recs = [dbdir ++ "/manifest"] ++ [dbdir ++ "/etcsums" | etc] ++ found
      sorted = reverse (uniq (sort recs))
  writeFileBin (dbdir ++ "/manifest") (concatMap (\r -> stripPrefixOr top r ++ "\n") sorted)
  where
    uniq (a : b : r) | a == b = uniq (b : r)
    uniq (a : r) = a : uniq r
    uniq [] = []

-- | @pkg_manifest_replace PKG OLD NEW@
pkgManifestReplace :: Env -> String -> String -> String -> IO ()
pkgManifestReplace e pkg old new = do
  tmp <- tmpFile e pkg ("manifest-replace-" ++ afterLast '/' old)
  let man = eSysDb e ++ "/" ++ pkg ++ "/manifest"
  c <- maybe "" id `fmap` readFileMaybe man
  let ls = [if l == old then new else l | l <- map read1 (readLoop c)]
  writeFileBin tmp (concatMap (++ "\n") (reverse (sort ls)))
  -- The temporary directory is in the cache, which may be on another
  -- filesystem than the root.
  run "mv" ["-f", tmp, man] >>= check

-- | @grep -lxF PATTERN FILES...@ ignoring unreadable files.
grepFiles :: String -> [FilePath] -> IO [FilePath]
grepFiles pat files = fmap concat $ forM files $ \f -> do
  c <- readFileMaybe f
  return [f | Just s <- [c], pat `elem` grepLines s]

-- | @pkg_owner PATH [FILES...]@: the last manifest naming PATH.
pkgOwner :: Env -> String -> Maybe [FilePath] -> IO (Maybe String)
pkgOwner e path mfiles = do
  files <- maybe (globIn (eSysDb e ++ "/") "*/manifest") return mfiles
  hits <- grepFiles path files
  case reverse hits of
    [] -> return Nothing
    (h : _) -> return (Just (afterLast '/' (beforeLast '/' h)))
