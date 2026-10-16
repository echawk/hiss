-- | Filesystem predicates matching @test(1)@ and byte-exact file IO.
module Kiss.FS
  ( tryIO, isDir, isFile, isLink, exists, lexists, isExec, isNonEmpty
  , getEntries, readFileStrict, readFileMaybe, writeFileBin, appendFileBin
  , removeIfExists, readHead
  ) where

import Control.Exception (IOException, try, evaluate)
import System.Directory
import System.IO

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

orFalse :: IO Bool -> IO Bool
orFalse act = either (const False) id `fmap` tryIO act

-- | @test -d@ (follows symlinks).
isDir :: FilePath -> IO Bool
isDir p = orFalse (doesDirectoryExist p)

-- | @test -f@ (follows symlinks).
isFile :: FilePath -> IO Bool
isFile p = orFalse (doesFileExist p)

-- | @test -h@
isLink :: FilePath -> IO Bool
isLink p = orFalse (pathIsSymbolicLink p)

-- | @test -e@ (follows symlinks, so a dangling symlink does not exist).
exists :: FilePath -> IO Bool
exists p = orFalse (doesPathExist p)

-- | @test -e || test -h@
lexists :: FilePath -> IO Bool
lexists p = do
  e <- exists p
  if e then return True else isLink p

-- | @test -x@
isExec :: FilePath -> IO Bool
isExec p = orFalse (do
  ps <- getPermissions p
  return (executable ps || searchable ps))

-- | @test -s@
isNonEmpty :: FilePath -> IO Bool
isNonEmpty p = orFalse (do
  f <- doesFileExist p
  if f then fmap (> 0) (getFileSize p) else return False)

-- | Directory entries, or none when the directory cannot be read.
getEntries :: FilePath -> IO [FilePath]
getEntries d = either (const []) id `fmap` tryIO (getDirectoryContents d)

readFileStrict :: FilePath -> IO String
readFileStrict p = withBinaryFile p ReadMode (\h -> do
  s <- hGetContents h
  _ <- evaluate (length s)
  return s)

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe p = either (const Nothing) Just `fmap` tryIO (readFileStrict p)

writeFileBin :: FilePath -> String -> IO ()
writeFileBin p s = withBinaryFile p WriteMode (\h -> hPutStr h s)

appendFileBin :: FilePath -> String -> IO ()
appendFileBin p s = withBinaryFile p AppendMode (\h -> hPutStr h s)

removeIfExists :: FilePath -> IO ()
removeIfExists p = do
  _ <- tryIO (removeFile p)
  return ()

-- | The first @n@ bytes of a file.
readHead :: Int -> FilePath -> IO String
readHead n p = either (const "") id `fmap` tryIO (withBinaryFile p ReadMode (\h -> do
  s <- hGetContents h
  let t = take n s
  _ <- evaluate (length t)
  return t))
