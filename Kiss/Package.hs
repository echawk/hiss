-- | Dependency traversal and explicit-argument ordering (kiss.el's
-- kiss-package).
module Kiss.Package (pkgDepends, pkgOrder) where

import Control.Monad (forM_, unless, when)
import Data.IORef

import Kiss.Env
import Kiss.FS
import Kiss.Glob
import Kiss.Search
import Kiss.Util

-- | @pkg_depends PKG MODE FILTER ANCESTORS DEP_TYPE@.  Depth-first postorder
-- into @deps@; MODE is @expl@ (never emitted itself), @raw@, or empty for an
-- implicit dependency.  @explicit@ is the explicit list built so far.
pkgDepends :: Env -> IORef [String] -> [String]
           -> String -> String -> String -> String -> String -> IO ()
pkgDepends e depsRef explicit p mode filt anc _dtype = do
  deps <- readIORef depsRef
  unless (p `elem` deps) $ do
    installed <- isDir (eSysDb e ++ "/" ++ p)
    let filtered = not (null filt) && null mode && p `notElem` explicit && installed
    unless filtered $ do
      let parent = afterLast ' ' anc
          a = " " ++ parent ++ " "
          b = " " ++ p ++ " "
      when (inOrder (concat (replicate 5 [a, b])) (" " ++ anc ++ " ")) $
        die e ("Circular dependency detected " ++ p ++ " <> " ++ parent) ""
      found <- pkgFindFirst e p "" Nothing
      case found of
        Nothing -> return ()
        Just dir -> do
          has <- exists (dir ++ "/depends")
          when has $ do
            c <- readFileMaybe (dir ++ "/depends")
            forM_ (maybe [] readLoopOk c) $ \l -> do
              let (dep, ty) = read2 l
              unless (null dep || take 1 dep == "#") $
                pkgDepends e depsRef explicit dep "" filt (anc ++ " " ++ p) ty
      when (mode /= "expl") $ modifyIORef depsRef (++ [p])

-- | @pkg_order ARGS...@: returns @order@ and its reverse @redro@.
pkgOrder :: Env -> [String] -> IO ([String], [String])
pkgOrder e pkgs = do
  depsRef <- newIORef []
  forM_ pkgs $ \p ->
    if globMatch "/*@*.tar.*" p then modifyIORef depsRef (++ [p])
    else if globMatch "*@*.tar.*" p then modifyIORef depsRef (++ [ePpwd e ++ "/" ++ p])
    else if globMatch "*/*" p then die e ("Not a package' (" ++ p ++ ")") ""
    else pkgDepends e depsRef [] p "raw" "" "" ""
  deps <- readIORef depsRef
  let keep d = d `elem` pkgs || stripPrefixOr (ePpwd e ++ "/") d `elem` pkgs
      order = filter keep (concatMap words deps)
  return (order, reverse order)
