-- | Process configuration (the reference's @main@ and @create_tmp_dirs@),
-- environment-variable access, and log output.
--
-- Exported environment variables are the shell's exported variables: the
-- reference mutates @KISS_PATH@, @KISS_FORCE@, @_KISS_LVL@ and @PWD@ and its
-- children observe the change, so this port stores them in the process
-- environment as well.
module Kiss.Env
  ( Env (..), pkgDb, choDb
  , getVar, getVarOr, setVar, unsetVar, getPWD
  , logK, log1, log2, war, die
  ) where

import Data.IORef
import System.Environment.Blank (getEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO

data Env = Env
  { eRoot       :: String   -- ^ normalized KISS_ROOT ("" is the host root)
  , eSysDb      :: FilePath
  , eSysCh      :: FilePath
  , eCacDir     :: FilePath
  , eSrcDir     :: FilePath
  , eLogDir     :: FilePath
  , eBinDir     :: FilePath
  , eProc       :: FilePath
  , eMakDir     :: FilePath
  , ePkgDir     :: FilePath
  , eTarDir     :: FilePath
  , eTmpDir     :: FilePath
  , eTmpBase    :: FilePath  -- ^ KISS_TMPDIR after defaulting
  , ePid        :: String
  , ePpwd       :: FilePath
  , eTime       :: String
  , eCompress   :: String
  , eSu         :: String
  , eElf        :: String
  , eSha        :: String
  , eGet        :: String
  , eSelf       :: FilePath
  , eC1, eC2, eC3 :: String
  , eTrap       :: IORef Bool    -- ^ INT/EXIT traps installed
  , eSafe       :: IORef Bool    -- ^ the reference's global @safe@ flag
  , eBuildCur   :: IORef Int     -- ^ @_build_cur@
  , ePreferCache :: IORef String -- ^ @prefer_cache@
  , eBuildInstall :: IORef String -- ^ @build_install@
  , eDispatch   :: [String] -> IO ()  -- ^ re-enter @args@ (for subshells)
  }

pkgDb, choDb :: String
pkgDb = "var/db/kiss/installed"
choDb = "var/db/kiss/choices"

getVar :: String -> IO String
getVar k = maybe "" id `fmap` getEnv k

-- | @${K:-default}@
getVarOr :: String -> String -> IO String
getVarOr k d = do
  v <- getVar k
  return (if null v then d else v)

setVar :: String -> String -> IO ()
setVar k v = setEnv k v True

unsetVar :: String -> IO ()
unsetVar = unsetEnv

-- | The shell's logical working directory.
getPWD :: IO FilePath
getPWD = getVar "PWD"

-- | @log MSG [MSG2] [PREFIX]@
logK :: Env -> String -> String -> String -> IO ()
logK e a b pre = do
  hFlush stdout
  hPutStr stderr (eC1 e ++ (if null pre then "->" else pre) ++ " "
                  ++ eC3 e ++ (if null b then "" else eC2 e) ++ a ++ eC3 e
                  ++ " " ++ b ++ "\n")

log1 :: Env -> String -> IO ()
log1 e a = logK e a "" ""

log2 :: Env -> String -> String -> IO ()
log2 e a b = logK e a b ""

war :: Env -> String -> String -> IO ()
war e a b = logK e a b "WARNING"

die :: Env -> String -> String -> IO a
die e a b = do
  logK e a b "ERROR"
  exitWith (ExitFailure 1)
