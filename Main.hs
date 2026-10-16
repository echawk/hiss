-- | hiss: a KISS package manager in Haskell.  Process initialization (the
-- reference's @main@ and @create_tmp_dirs@) followed by command dispatch.
module Main (main) where

import Control.Monad (forM_, when)
import Data.IORef
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding)
import System.Directory (canonicalizePath, createDirectoryIfMissing, findExecutable, getCurrentDirectory)
import System.Environment (getArgs, getExecutablePath)
import qualified System.Environment.Blank as Blank
import System.Exit (ExitCode (..), exitWith)
import System.IO
import System.Process (getCurrentPid)

import Kiss (dispatch)
import Kiss.Env
import Kiss.FS
import Kiss.Hook
import Kiss.OS
import Kiss.Util

-- | @command -v A || command -v B ...@
firstAvailable :: [String] -> IO (Maybe String)
firstAvailable [] = return Nothing
firstAvailable (c : cs) = do
  p <- findExecutable c
  maybe (firstAvailable cs) (return . Just) p

-- | Assign a shell variable; it reaches children only if it was imported
-- from (and so is exported to) the environment.
assign :: String -> String -> IO ()
assign k v = do
  present <- Blank.getEnv k
  case present of
    Just _ -> setVar k v
    Nothing -> return ()

-- | The shell's initial @PWD@: the inherited value when it names the
-- current directory, otherwise the physical path.
initPWD :: IO FilePath
initPWD = do
  cwd <- getCurrentDirectory
  inherited <- getVar "PWD"
  ok <- if take 1 inherited /= "/" then return False else do
    a <- tryIO (canonicalizePath inherited)
    b <- tryIO (canonicalizePath cwd)
    return (case (a, b) of
      (Right x, Right y) -> x == y
      _ -> False)
  let pwd = if ok then inherited else cwd
  setVar "PWD" pwd
  return pwd

main :: IO ()
main = do
  setFileSystemEncoding char8
  setLocaleEncoding char8
  setForeignEncoding char8
  forM_ [stdin, stdout, stderr] (\h -> hSetBinaryMode h True)
  hSetBuffering stdout LineBuffering
  argv <- getArgs

  color <- getVar "KISS_COLOR"
  tty <- hIsTerminalDevice stderr
  let (c1, c2, c3) = if color /= "0" && tty
        then ("\ESC[1;33m", "\ESC[1;34m", "\ESC[m") else ("", "", "")
  ppwd <- initPWD

  compress <- getVarOr "KISS_COMPRESS" "gz"
  assign "KISS_COMPRESS" compress
  ownPid <- getCurrentPid
  pid <- getVarOr "KISS_PID" (show ownPid)
  assign "KISS_PID" pid
  logname <- getVar "LOGNAME"
  when (null logname) $ do
    hPutStrLn stderr "hiss: LOGNAME: POSIX requires LOGNAME be set"
    exitWith (ExitFailure 1)

  suEnv <- getVar "KISS_SU"
  su <- if null suEnv then fmap (maybe "su" id) (firstAvailable ["ssu", "sudo", "doas", "su"])
                      else return suEnv
  elfEnv <- getVar "KISS_ELF"
  elf <- if null elfEnv then fmap (maybe "ldd" id) (firstAvailable ["readelf", "eu-readelf", "llvm-readelf"])
                        else return elfEnv
  chkEnv <- getVar "KISS_CHK"
  sha <- if null chkEnv then firstAvailable ["openssl", "sha256sum", "sha256", "shasum", "digest"]
                        else return (Just chkEnv)
  getEnv' <- getVar "KISS_GET"
  get <- if null getEnv' then firstAvailable ["aria2c", "axel", "curl", "wget", "wget2"]
                         else return (Just getEnv')
  dateEnv <- getVar "_KISS_DATE"
  time <- if null dateEnv
    then do
      (_, out) <- capture defSpec "date" ["+%Y-%m-%d-%H:%M"]
      return (takeWhile (/= '\n') out)
    else return dateEnv

  rootEnv <- getVar "KISS_ROOT"
  let root = stripTrailing '/' rootEnv
  assign "KISS_ROOT" root
  _ <- tryIO (createDirectoryIfMissing True (root ++ "/"))
  xdg <- getVar "XDG_CACHE_HOME"
  home <- getVar "HOME"
  let cac0 = if null xdg then stripTrailing '/' home ++ "/.cache" else xdg
      cac = stripTrailing '/' cac0 ++ "/kiss"
  tmpEnv <- getVar "KISS_TMPDIR"
  let tmpBase = if null tmpEnv then cac ++ "/proc" else tmpEnv
  assign "KISS_TMPDIR" tmpBase
  let procDir = stripTrailing '/' tmpBase ++ "/" ++ pid
  self <- getExecutablePath
  trapRef <- newIORef False
  safeRef <- newIORef False
  curRef <- newIORef 0
  pcRef <- newIORef ""
  biRef <- newIORef ""
  let e = Env
        { eRoot = root
        , eSysDb = root ++ "/" ++ pkgDb
        , eSysCh = root ++ "/" ++ choDb
        , eCacDir = cac
        , eSrcDir = cac ++ "/sources"
        , eLogDir = cac ++ "/logs/" ++ beforeLast '-' time
        , eBinDir = cac ++ "/bin"
        , eProc = procDir
        , eMakDir = procDir ++ "/build"
        , ePkgDir = procDir ++ "/pkg"
        , eTarDir = procDir ++ "/extract"
        , eTmpDir = procDir ++ "/tmp"
        , eTmpBase = tmpBase
        , ePid = pid
        , ePpwd = ppwd
        , eTime = time
        , eCompress = compress
        , eSu = su
        , eElf = elf
        , eSha = maybe "" id sha
        , eGet = maybe "" id get
        , eSelf = self
        , eC1 = c1, eC2 = c2, eC3 = c3
        , eTrap = trapRef
        , eSafe = safeRef
        , eBuildCur = curRef
        , ePreferCache = pcRef
        , eBuildInstall = biRef
        , eDispatch = dispatch e
        }
  when (sha == Nothing) $ war e "No sha256 utility found" ""
  when (get == Nothing) $ die e "No download utility found (aria2c, axel, curl, wget, wget2)" ""
  forM_ [eSrcDir e, eLogDir e, eBinDir e, eMakDir e, ePkgDir e, eTarDir e, eTmpDir e] $ \d -> do
    r <- tryIO (createDirectoryIfMissing True d)
    case r of
      Left _ -> do
        hPutStrLn stderr ("mkdir: " ++ d ++ ": cannot create directory")
        exitWith (ExitFailure 1)
      Right () -> return ()
  trapOn e
  code <- guarded e (dispatch e argv)
  traps <- readIORef (eTrap e)
  final <- if traps then exitTrap e code else return code
  hFlush stdout
  exitWith final
