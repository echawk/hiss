-- | Alternatives (choices) listing and swapping, and the preferred-provider
-- query (kiss.el's kiss-alternatives and kiss-preferred).
module Kiss.Alternatives
  ( alternativesList, pkgAlternatives, pkgPreferred, pkgSwap
  ) where

import Control.Monad (forM, forM_, unless, when)
import System.IO

import Kiss.Env
import Kiss.FS
import Kiss.Glob
import Kiss.Manifest
import Kiss.OS
import Kiss.Util

-- | Records printed by @kiss alternatives@, in pathname-expansion order.
-- An empty choices directory leaves the pattern literal: @* /*@.
alternativesList :: Env -> IO [String]
alternativesList e = do
  entries <- globIn (eSysCh e ++ "/") "*"
  return [let n = replaceAll ">" "/" (afterLast '/' p)
          in beforeFirst '/' n ++ " /" ++ afterFirst '/' n | p <- entries]

-- | @pkg_alternatives ARGS...@
pkgAlternatives :: Env -> [String] -> IO ()
pkgAlternatives e args = case args of
  ("-" : _) -> do
    c <- hGetContents stdin
    forM_ (readLoop c) $ \l -> let (p, path) = read2 l in pkgSwap e p path
  (p : rest) | not (null p) -> pkgSwap e p (case rest of
                                             (x : _) -> x
                                             [] -> "")
  _ -> alternativesList e >>= mapM_ putStrLn

-- | @pkg_preferred [PKG]@: print and return active providers of paths that
-- have inactive choices.
pkgPreferred :: Env -> [String] -> IO [String]
pkgPreferred e args = do
  old <- getPWD
  cd (eSysDb e)
  files <- case args of
    (p : _) | not (null p) -> do
      pd <- isDir p
      unless pd $ die e ("'" ++ p ++ "' not found") ""
      return [p ++ "/manifest"]
    _ -> glob "*/manifest"
  alts <- alternativesList e
  let pats = mkSet (map (afterFirst ' ') alts)
  recs <- fmap concat $ forM (files ++ ["/dev/null"]) $ \f -> do
    c <- readFileMaybe f
    case c of
      Nothing -> do
        hPutStrLn stderr ("grep: " ++ f ++ ": No such file or directory")
        return []
      Just s -> return [sed (f ++ ":" ++ l) | l <- grepLines s, l `member` pats]
  mapM_ putStrLn recs
  cd old
  return recs
  where
    sed s = maybe s id (subst s)
    subst s = go "" s
      where
        go _ [] = Nothing
        go acc r@(c : cs)
          | take 10 r == "/manifest:" = Just (reverse acc ++ " " ++ drop 10 r)
          | otherwise = go (c : acc) cs

-- | @pkg_swap PKG PATH@
pkgSwap :: Env -> String -> String -> IO ()
pkgSwap e p path = do
  let root = eRoot e
  d <- isDir (eSysDb e ++ "/" ++ p)
  unless d $ die e ("'" ++ p ++ "' not found") ""
  let enc = replaceAll "/" ">" (p ++ path)
      choice = eSysCh e ++ "/" ++ enc
  f <- isFile choice
  l <- isLink choice
  unless (f || l) $
    die e ("Alternative '" ++ p ++ " " ++ (if null path then "null" else path) ++ "' doesn't exist") ""
  active <- isFile (root ++ path)
  when active $ do
    mo <- pkgOwner e ("/" ++ dropOneLeading '/' path) Nothing
    owner <- maybe (die e ("File '" ++ path ++ "' exists on filesystem but isn't owned") "") return mo
    log1 e ("Swapping '" ++ path ++ "' from '" ++ owner ++ "' to '" ++ p ++ "'")
    let saved = owner ++ ">" ++ afterFirst '>' enc
    run "cp" ["-Pf", root ++ path, eSysCh e ++ "/" ++ saved] >>= check
    sl <- isLink (root ++ path)
    unless sl $ run "touch" ["-r", root ++ path, eSysCh e ++ "/" ++ saved] >>= check
    pkgManifestReplace e owner path ("/" ++ choDb ++ "/" ++ saved)
  run "mv" ["-f", choice, root ++ "/" ++ path] >>= check
  pkgManifestReplace e p ("/" ++ choDb ++ "/" ++ enc) path
