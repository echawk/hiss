-- | Permission conversion, digests, binary classification, and archive
-- helpers (kiss.el's kiss-file).
module Kiss.File
  ( fileRwx, b3, b3raw, sh256, odRender, stripPatterns, isStripCandidate
  , decompressTo, extractArchive, createArchive, pid'
  ) where

import Control.Monad (filterM)
import Numeric (showOct)
import System.Exit (ExitCode (..), exitWith)

import Kiss.Env
import Kiss.FS
import Kiss.Glob
import Kiss.OS

-- | @file_rwx@: the octal mode of @ls -ld@'s permission string.  Like the
-- reference, setuid and setgid are dropped and only the sticky bit is kept.
fileRwx :: FilePath -> IO String
fileRwx p = do
  (_, out) <- capture defSpec "ls" ["-ld", p]
  return (convert (drop 1 (take 10 out)))
  where
    convert perms = go perms (zip [1 :: Int ..] [4, 2, 1, 4, 2, 1, 4, 2, 1]) "" 0
    go _ [] oct _ = oct
    go rwx ((pos, v) : rest) oct o =
      let c = take 1 rwx
          o' = o + (if c `elem` ["r", "w", "x"] then v
                    else if c `elem` ["s", "t"] then 1 else 0)
          oct1 = if c `elem` ["t", "T"] then '1' : oct else oct
          (oct2, o2) = if pos `mod` 3 == 0 then (oct1 ++ show o', 0) else (oct1, o')
      in go (drop 1 rwx) rest oct2 o2

-- | Parse digest-tool output into one digest per line (@${sum%% *}@).
digests :: String -> [String]
digests out = [takeWhile (/= ' ') l | l <- lines out, not (null l)]

-- | Keep arguments that exist and are not directories.
hashable :: [FilePath] -> IO [FilePath]
hashable = filterM (\f -> do
  d <- isDir f
  x <- exists f
  return (not d && x))

-- | @b3@: BLAKE3 digests of existing non-directory files.
b3 :: Env -> [FilePath] -> IO [String]
b3 e fs = hashable fs >>= b3raw e

-- | @_b3@
b3raw :: Env -> [FilePath] -> IO [String]
b3raw _ [] = return []
b3raw e fs = do
  (c, out) <- capture defSpec "b3sum" (["-l", "33"] ++ fs)
  case c of
    ExitSuccess -> return (digests out)
    _ -> die e "Failed to generate checksums" ""

-- | @sh256@: legacy SHA-256 digests using the configured utility.
sh256 :: Env -> [FilePath] -> IO [String]
sh256 e fs0 = do
  fs <- hashable fs0
  if null fs then return [] else do
    let args = case reverse (takeWhile (/= '/') (reverse (eSha e))) of
          "openssl" -> ["dgst", "-sha256", "-r"] ++ fs
          "sha256" -> ["-r"] ++ fs
          "shasum" -> ["-a", "256"] ++ fs
          "digest" -> ["-a", "sha256"] ++ fs
          _ -> fs
    (c, out) <- capture defSpec (eSha e) args
    case c of
      ExitSuccess -> return (digests out)
      _ -> die e "Failed to generate checksums" ""

-- | Render bytes as @od -A o -t c@ does, so the reference's loose ELF and
-- archive case patterns can be applied verbatim.
odRender :: String -> String
odRender bytes = concatMap line (chunks 0 bytes) ++ offset (length bytes) ++ "\n"
  where
    chunks _ [] = []
    chunks n s = (n, take 16 s) : chunks (n + 16) (drop 16 s)
    line (n, cs) = offset n ++ concatMap field cs ++ "\n"
    offset n = let o = showOct n "" in replicate (7 - length o) '0' ++ o
    field c = let t = render c in replicate (4 - length t) ' ' ++ t
    render c = case c of
      '\0' -> "\\0"
      '\a' -> "\\a"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\v' -> "\\v"
      _ | c >= ' ' && c <= '~' -> [c]
        | otherwise -> let o = showOct (fromEnum c) "" in replicate (3 - length o) '0' ++ o

-- | Manifest locations examined for stripping and dependency detection.
stripPatterns :: [String]
stripPatterns =
  [ "*/sbin/?*[!/]", "*/bin/?*[!/]", "*/lib/?*[!/]"
  , "*/lib??/?*[!/]", "*/lib???/?*[!/]", "*/lib????/?*[!/]" ]

isStripCandidate :: String -> Bool
isStripCandidate f = any (`globMatch` f) stripPatterns

decompressScript :: String
decompressScript =
  "case $1 in\n\
  \  *.tar) cat ;;\n\
  \  *.tbz|*.bz2) bzip2 -dc ;;\n\
  \  *.lz) lzip -dc ;;\n\
  \  *.tgz|*.gz) gzip -dc ;;\n\
  \  *.lzma) lzma -dcT0 ;;\n\
  \  *.xz|*.txz) xz -dcT0 ;;\n\
  \  *.zst) zstd -dcT0 ;;\n\
  \esac < \"$1\""

-- | @decompress FILE > OUT@
decompressTo :: FilePath -> FilePath -> IO ExitCode
decompressTo file out = sh (decompressScript ++ " > \"$2\"") [file, out]

-- | @decompress FILE | tar xf -@ in the current directory.
extractArchive :: FilePath -> IO ExitCode
extractArchive file = sh ("{ " ++ decompressScript ++ "; } | tar xf -") [file]

-- | @tar cf - . | compress > ARCHIVE@ in the current directory.
createArchive :: String -> FilePath -> IO ExitCode
createArchive method out = sh
  "tar cf - . | case $1 in\n\
  \  bz2) bzip2 -c ;;\n\
  \  gz) gzip -c ;;\n\
  \  lz) lzip -c ;;\n\
  \  lzma) lzma -cT0 ;;\n\
  \  xz) xz -cT0 ;;\n\
  \  zst) zstd -cT0 ;;\n\
  \esac > \"$2\"" [method, out]

pid' :: Env -> String
pid' = ePid
