-- | Process, working-directory, identity, and temporary-file boundary.
module Kiss.OS
  ( Spec (..), defSpec, spawn, run, runQuiet, capture, sh, check, exitCode
  , withDevNull, cd, mkcd, lsOwner, amOwner, asUser
  , pkgClean, trapOn, trapOff, prompt, tmpFile, tmpFileCopy
  , currentEnvWith, StdStream (..)
  ) where

import Control.Monad (unless, when)
import Data.IORef
import Data.List (intercalate)
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitWith)
import System.IO
import System.IO.Error (isEOFError, isDoesNotExistError, isPermissionError)
import System.Process

import Kiss.Env
import Kiss.FS
import Kiss.Util

data Spec = Spec
  { sEnv :: Maybe [(String, String)]
  , sIn  :: StdStream
  , sOut :: StdStream
  , sErr :: StdStream
  }

defSpec :: Spec
defSpec = Spec Nothing Inherit Inherit Inherit

-- | Run a program directly (no shell), as the reference does for most
-- helpers.  An execution failure behaves like the shell's 126/127 status.
spawn :: Spec -> FilePath -> [String] -> IO ExitCode
spawn sp cmd args = do
  hFlush stdout
  hFlush stderr
  r <- tryIO (createProcess (proc cmd args)
         { env = sEnv sp, std_in = sIn sp, std_out = sOut sp, std_err = sErr sp })
  case r of
    Left err -> execFailure cmd err
    Right (_, _, _, ph) -> waitForProcess ph

execFailure :: String -> IOError -> IO ExitCode
execFailure cmd err = do
  hPutStrLn stderr (cmd ++ ": " ++ (if isDoesNotExistError err then "not found"
                                    else if isPermissionError err then "Permission denied"
                                    else show err))
  return (ExitFailure (if isDoesNotExistError err then 127 else 126))

run :: FilePath -> [String] -> IO ExitCode
run = spawn defSpec

-- | Run with standard error discarded (@2>/dev/null@).
runQuiet :: FilePath -> [String] -> IO ExitCode
runQuiet cmd args = withDevNull (\n -> spawn defSpec { sErr = UseHandle n } cmd args)

-- | Command substitution: capture standard output.
capture :: Spec -> FilePath -> [String] -> IO (ExitCode, String)
capture sp cmd args = do
  hFlush stdout
  hFlush stderr
  r <- tryIO (createProcess (proc cmd args)
         { env = sEnv sp, std_in = sIn sp, std_out = CreatePipe, std_err = sErr sp })
  case r of
    Left err -> do
      c <- execFailure cmd err
      return (c, "")
    Right (_, Just hout, _, ph) -> do
      hSetBinaryMode hout True
      out <- hGetContents hout
      _ <- return $! length out
      c <- waitForProcess ph
      hClose hout
      return (c, out)
    Right (_, Nothing, _, ph) -> do
      c <- waitForProcess ph
      return (c, "")

-- | Run a fixed shell fragment with positional parameters.  Only constant
-- scripts are passed here; data travels as arguments, never through eval.
sh :: String -> [String] -> IO ExitCode
sh script args = run "/bin/sh" (["-c", script, "sh"] ++ args)

exitCode :: ExitCode -> Int
exitCode ExitSuccess = 0
exitCode (ExitFailure n) = n

-- | @set -e@: a failed command terminates the process with its status.
check :: ExitCode -> IO ()
check ExitSuccess = return ()
check c = exitWith c

withDevNull :: (Handle -> IO a) -> IO a
withDevNull = withBinaryFile "/dev/null" ReadWriteMode

currentEnvWith :: [(String, String)] -> IO [(String, String)]
currentEnvWith over = do
  cur <- getEnvironment
  return (over ++ [kv | kv@(k, _) <- cur, k `notElem` map fst over])

-- | @cd DIR@, maintaining the logical @PWD@ and @OLDPWD@ that the shell
-- exports to hooks and build scripts.
cd :: FilePath -> IO ()
cd p = do
  r <- tryIO (setCurrentDirectory p)
  case r of
    Left _ -> do
      hPutStrLn stderr ("cd: can't cd to " ++ p)
      exitWith (ExitFailure 1)
    Right () -> do
      old <- getPWD
      setVar "OLDPWD" old
      setVar "PWD" (logical old p)

logical :: FilePath -> FilePath -> FilePath
logical old p = normalize (if take 1 p == "/" then p else old ++ "/" ++ p)
  where
    normalize s = "/" ++ intercalate "/" (reverse (foldl step [] (splitOn '/' s)))
    step acc "" = acc
    step acc "." = acc
    step acc ".." = drop 1 acc
    step acc c = c : acc

-- | @mkdir -p "$@" && cd "$1"@
mkcd :: [FilePath] -> IO ()
mkcd dirs = do
  mapM_ mk dirs
  case dirs of
    (d : _) -> cd d
    [] -> return ()
  where
    mk d = do
      r <- tryIO (createDirectoryIfMissing True d)
      case r of
        Left _ -> do
          hPutStrLn stderr ("mkdir: " ++ d ++ ": cannot create directory")
          exitWith (ExitFailure 1)
        Right () -> return ()

-- | The owner field of @ls -ld@.
lsOwner :: FilePath -> IO (Maybe String)
lsOwner p = do
  (c, out) <- capture defSpec "ls" ["-ld", p]
  case (c, words out) of
    (ExitSuccess, _ : _ : user : _) -> return (Just user)
    _ -> return Nothing

-- | @am_owner@: whether LOGNAME owns the path, and the owner's name.
amOwner :: Env -> FilePath -> IO (Bool, String)
amOwner e p = do
  mu <- lsOwner p
  case mu of
    Nothing -> die e ("Failed to file information for '" ++ p ++ "'") ""
    Just user -> do
      me <- getVar "LOGNAME"
      return (me == user, user)

-- | @as_user CMD...@
asUser :: Env -> String -> [String] -> IO ExitCode
asUser e user args = do
  putStrLn ("Using " ++ eSu e ++ " (to become " ++ user ++ ")")
  case afterLast '/' (eSu e) of
    "su" -> sh "exec \"$0\" -c \"$1\" \"$2\" 3<&0 </dev/tty"
              [eSu e, unwords args ++ " <&3", user]
    _ -> run (eSu e) (["-u", user, "--"] ++ args)

-- | @pkg_clean@
pkgClean :: Env -> IO ()
pkgClean e = do
  dbg <- getVarOr "KISS_DEBUG" "0"
  lvl <- getVarOr "_KISS_LVL" "1"
  when (dbg == "0") $
    if lvl == "1" then rmrf (eProc e) else rmrf (eTarDir e)
  where rmrf d = tryIO (removePathForcibly d) >> return ()

trapOn, trapOff :: Env -> IO ()
trapOn e = writeIORef (eTrap e) True
trapOff e = writeIORef (eTrap e) False

-- | @prompt [MSG]@
prompt :: Env -> String -> IO ()
prompt e msg = do
  unless (null msg) (log1 e msg)
  log1 e "Continue?: Press Enter to continue or Ctrl+C to abort"
  p <- getVar "KISS_PROMPT"
  unless (p == "0") $ do
    -- dd bs=1 count=1: read exactly one byte; EOF is not an error for dd.
    hSetBuffering stdin NoBuffering
    r <- tryIO (hGetChar stdin)
    case r of
      Left err | isEOFError err -> return ()
               | otherwise -> exitWith (ExitFailure 1)
      Right _ -> return ()

-- | @tmp_file NAME SUFFIX@
tmpFile :: Env -> String -> String -> IO FilePath
tmpFile e name suffix = do
  let p = eTmpDir e ++ "/" ++ name ++ "-" ++ suffix
  r <- tryIO (writeFileBin p "")
  case r of
    Left _ -> die e name "Failed to create temporary file"
    Right () -> return p

-- | @tmp_file_copy NAME SUFFIX SOURCE@
tmpFileCopy :: Env -> String -> String -> FilePath -> IO FilePath
tmpFileCopy e name suffix src = do
  p <- tmpFile e name suffix
  f <- isFile src
  when f $ do
    c <- readFileMaybe src
    maybe (return ()) (writeFileBin p) c
  return p
