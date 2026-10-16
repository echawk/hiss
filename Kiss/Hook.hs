-- | External (@KISS_HOOK@) and package-local hooks, plus the trap and
-- subshell semantics that decide when the exit hooks run.
module Kiss.Hook
  ( runHook, runHookPkg, guarded, exitTrap, subshell
  ) where

import Control.Exception
import Control.Monad (forM_, unless, when)
import Data.IORef
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitWith)
import System.IO

import Kiss.Env
import Kiss.FS
import Kiss.OS
import Kiss.Util

-- | @run_hook EVENT ARGS...@
runHook :: Env -> [String] -> IO ()
runHook e args = do
  hooks <- getVar "KISS_HOOK"
  forM_ (splitColon hooks) $ \h -> do
    c <- run h args
    unless (c == ExitSuccess) $
      die e (head (args ++ [""]) ++ " hook failed: '" ++ h ++ "'") ""

-- | @run_hook_pkg HOOK PKG@: run an installed package's hook in a chroot.
runHookPkg :: Env -> String -> String -> IO ()
runHookPkg e hook pkg = do
  let hp = eSysDb e ++ "/" ++ pkg ++ "/" ++ hook
  x <- isExec hp
  if x
    then do
      log2 e pkg ("Running " ++ hook ++ " hook")
      me <- getVar "LOGNAME"
      let cmd = [ "env", "KISS_ROOT=", "chroot"
                , if null (eRoot e) then "/" else eRoot e
                , stripPrefixOr (eRoot e) hp ]
      c <- if me == "root" then run (head cmd) (tail cmd) else asUser e "root" cmd
      check c
    else do
      f <- isFile hp
      when f $ war e pkg ("skipping " ++ hook ++ " hook: not executable")

-- | Run an action, converting its termination into an exit status.  A
-- handled interrupt runs the @SIGINT@ hook when traps are installed.
guarded :: Env -> IO () -> IO ExitCode
guarded e act = (act >> return ExitSuccess) `catch` handler
  where
    handler :: SomeException -> IO ExitCode
    handler ex = case fromException ex of
      Just c -> return c
      Nothing -> case fromException ex of
        Just UserInterrupt -> do
          t <- readIORef (eTrap e)
          if t
            then do
              _ <- guarded e (runHook e ["SIGINT"])
              return (ExitFailure 1)
            else return (ExitFailure 130)
        _ -> do
          hPutStrLn stderr ("hiss: " ++ show ex)
          return (ExitFailure 1)

-- | @trap_EXIT@: clean up, then run the @SIGEXIT@ hook.  A failing hook
-- replaces the exit status, as @exit@ inside a shell EXIT trap does.
exitTrap :: Env -> ExitCode -> IO ExitCode
exitTrap e code = do
  writeIORef (eTrap e) False
  pkgClean e
  c <- guarded e (runHook e ["SIGEXIT"])
  return (if c == ExitSuccess then code else c)

-- | A @( ... )@ subshell.  Traps are reset on entry, so an EXIT trap runs at
-- the end only if the subshell itself installed one; working directory,
-- exported variables and global flags do not leak back to the parent.
subshell :: Env -> IO () -> IO ExitCode
subshell e act = do
  envSaved <- getEnvironment
  cwdSaved <- tryIO getCurrentDirectory
  trapSaved <- readIORef (eTrap e)
  safeSaved <- readIORef (eSafe e)
  curSaved <- readIORef (eBuildCur e)
  pcSaved <- readIORef (ePreferCache e)
  biSaved <- readIORef (eBuildInstall e)
  writeIORef (eTrap e) False
  code <- guarded e act
  t <- readIORef (eTrap e)
  code' <- if t then exitTrap e code else return code
  envNow <- getEnvironment
  mapM_ unsetVar [k | (k, _) <- envNow, k `notElem` map fst envSaved]
  mapM_ (\(k, v) -> setVar k v) envSaved
  case cwdSaved of
    Right d -> tryIO (setCurrentDirectory d) >> return ()
    Left _ -> return ()
  writeIORef (eTrap e) trapSaved
  writeIORef (eSafe e) safeSaved
  writeIORef (eBuildCur e) curSaved
  writeIORef (ePreferCache e) pcSaved
  writeIORef (eBuildInstall e) biSaved
  return code'
