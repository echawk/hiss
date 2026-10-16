-- | Public commands and argument handling (kiss.el's kiss.el and the
-- reference's @args@).
module Kiss (dispatch, pkgListVersion) where

import Control.Monad (forM_, unless, when)
import Data.IORef
import System.Exit (ExitCode (..), exitWith)
import System.IO

import Kiss.Alternatives
import Kiss.Build
import Kiss.Env
import Kiss.FS
import Kiss.Glob
import Kiss.Install
import Kiss.OS
import Kiss.Package
import Kiss.Search
import Kiss.Source
import Kiss.Update
import Kiss.Util

-- | @pkg_list_version [PKG...]@
pkgListVersion :: Env -> [String] -> IO ()
pkgListVersion e args = do
  names <- case args of
    (a : _) | not (null a) -> return args
    _ -> globIn (eSysDb e ++ "/") "*"
  forM_ names $ \n -> do
    r <- pkgFindVersion e (afterLast '/' n) (Just (eSysDb e))
    putStrLn (rName r ++ " " ++ rVer r ++ "-" ++ rRel r)

usage :: Env -> IO ()
usage e = do
  mapM_ (log1 e)
    [ "kiss [a|b|c|d|i|l|p|r|s|u|U|v] [pkg]..."
    , "alternatives List and swap alternatives"
    , "build        Build packages"
    , "checksum     Generate checksums"
    , "download     Download sources"
    , "install      Install packages"
    , "list         List installed packages"
    , "preferred    List owners of files with alternatives"
    , "remove       Remove packages"
    , "search       Search for packages"
    , "update       Update the repositories"
    , "upgrade      Update the system"
    , "version      Package manager version" ]
  putStr "\nRun \"kiss [H|help-ext]\" to see all actions\n"

-- | @args ACTION ARGS...@
dispatch :: Env -> [String] -> IO ()
dispatch e argv = do
  let (action, args0) = case argv of
        [] -> ("", [])
        (a : r) -> (a, r)
      first = take 1 action
  args <-
    if action `elem` ["a", "alternatives", "p", "preferred"] then do
      let a1 = case args0 of
            (x : _) -> x
            [] -> ""
      when (any (`elem` "*![ ]/\n") a1) $
        die e ("Invalid argument: '!*[ ]/\\n' (" ++ a1 ++ ")") ""
      return args0
    else if action `elem` [ "b", "build", "c", "checksum", "d", "download"
                          , "i", "install", "l", "list", "r", "remove" ] then do
      forM_ args0 $ \a ->
        if first == "i"
          then when (any (`elem` "!*[ ]\n") a) $
                 die e ("Invalid argument: '!*[ ]\\n' ('" ++ a ++ "')") ""
          else when (any (`elem` "!*[ ]/\n") a) $
                 die e ("Invalid argument: '!*[ ]/\\n' ('" ++ a ++ "')") ""
      args1 <- if first /= "l" && null args0
        then do
          pwd <- getPWD
          kp <- getVar "KISS_PATH"
          setVar "KISS_PATH" (beforeLast '/' pwd ++ ":" ++ kp)
          return [afterLast '/' pwd]
        else return args0
      when (action `elem` ["r", "remove"]) $ do
        kp <- getVar "KISS_PATH"
        setVar "KISS_PATH" (eSysDb e ++ ":" ++ kp)
      (order, _) <- pkgOrder e args1
      return order
    else return args0
  lvl <- getVar "_KISS_LVL"
  setVar "_KISS_LVL" (show (readInt lvl + 1))
  escalated <- if action `elem` ["a", "alternatives", "i", "install", "r", "remove"]
                  && not (null (take 1 args) || null (head args))
    then do
      (own, user) <- amOwner e (eRoot e ++ "/")
      if own then return False else do
        trapOff e
        cd (ePpwd e)
        envArgs <- reexecEnv e user
        c <- asUser e user (["env"] ++ envArgs ++ [eSelf e, action] ++ args)
        check c
        trapOn e
        return True
    else return False
  unless escalated $ case action of
    _ | action `elem` ["a", "alternatives"] -> pkgAlternatives e args
      | action `elem` ["b", "build"] -> pkgBuildAll e args
      | action `elem` ["c", "checksum"] -> mapM_ (pkgChecksum e) args
      | action `elem` ["d", "download"] -> mapM_ (\p -> pkgSource e p "" >> return ()) args
      | action `elem` ["H", "help-ext"] -> pkgHelpExt e
      | action `elem` ["i", "install"] -> mapM_ (pkgInstall e) args
      | action `elem` ["l", "list"] -> pkgListVersion e args
      | action `elem` ["p", "preferred"] -> pkgPreferred e args >> return ()
      | action `elem` ["r", "remove"] -> mapM_ (pkgRemove e) (reverse args)
      | action `elem` ["s", "search"] ->
          forM_ args $ \q -> do
            rs <- pkgFindAll e q "" Nothing
            when (null rs) $ die e ("'" ++ q ++ "' not found") ""
            mapM_ putStrLn rs
      | action `elem` ["u", "update"] -> pkgUpdate e
      | action `elem` ["U", "upgrade"] -> pkgUpgrade e
      | action `elem` ["v", "version"] -> putStrLn "6.2.0"
      | null action -> usage e
      | otherwise -> extension e action args

readInt :: String -> Int
readInt s = case reads s of
  [(n, "")] -> n
  _ -> 0

-- | The fixed environment passed through privilege escalation.
reexecEnv :: Env -> String -> IO [String]
reexecEnv e user = do
  let var k = getVar k >>= \v -> return (k ++ "=" ++ v)
  vs <- mapM var ["HOME", "XDG_CACHE_HOME", "KISS_CHOICE", "KISS_COLOR", "KISS_FORCE", "KISS_HOOK", "KISS_PATH"]
  lvl <- getVar "_KISS_LVL"
  return ([ "LOGNAME=" ++ user ] ++ take 4 vs
          ++ [ "KISS_COMPRESS=" ++ eCompress e ] ++ drop 4 vs
          ++ [ "KISS_PID=" ++ ePid e, "KISS_ROOT=" ++ eRoot e
             , "KISS_TMPDIR=" ++ eTmpBase e, "_KISS_DATE=" ++ eTime e
             , "_KISS_LVL=" ++ lvl ])

-- | Run the first @kiss-ACTION*@ executable in @PATH@.
extension :: Env -> String -> [String] -> IO ()
extension e action args = do
  path <- getVar "PATH"
  ext <- pkgFind e ("kiss-" ++ action ++ "*") "-x" (Just path)
  env' <- currentEnvWith [("_KISS_LVL", "0"), ("KISS_PID", "")]
  c <- spawn defSpec { sEnv = Just env' } ext args
  check c
  hFlush stdout
  _ <- readIORef (eTrap e)
  return ()
