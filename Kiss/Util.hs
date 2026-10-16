-- | String helpers that reproduce the POSIX shell idioms used by the KISS
-- reference implementation: parameter expansions, @read -r@ field
-- splitting, and word lists.
module Kiss.Util
  ( isIfsWs, trimWs, read1, read2
  , readLoop, readLoopOk, grepLines
  , afterLast, beforeLast, afterFirst, beforeFirst
  , afterLastAny, beforeLastAny
  , stripTrailing, dropOneLeading, dropOneTrailing
  , stripPrefixOr, replaceAll, replaceChar, splitOn, splitColon
  , findSub, inOrder, isPrefixOf', isSuffixOf'
  , Set, mkSet, member
  ) where

import Data.List (isPrefixOf, isSuffixOf, sort)

-- | Characters in the default @IFS@ value.
isIfsWs :: Char -> Bool
isIfsWs c = c == ' ' || c == '\t' || c == '\n'

trimWs :: String -> String
trimWs = reverse . dropWhile isIfsWs . reverse . dropWhile isIfsWs

-- | @read -r var@ applied to one record.
read1 :: String -> String
read1 = trimWs

-- | @read -r first rest@ applied to one record.
read2 :: String -> (String, String)
read2 l = (a, trimWs r)
  where (a, r) = break isIfsWs (dropWhile isIfsWs l)

-- | Split file contents into newline-terminated records plus an optional
-- unterminated final record.
records :: String -> ([String], Maybe String)
records "" = ([], Nothing)
records s = case break (== '\n') s of
  (l, _ : rest) -> let (ls, f) = records rest in (l : ls, f)
  (l, [])       -> ([], Just l)

-- | Records seen by @while read -r ...; do@.  A final record without a
-- trailing newline makes @read@ fail, so it is never processed.
readLoop :: String -> [String]
readLoop = fst . records

-- | Records seen by @while read -r x ... || ok "$x"; do@: an unterminated
-- final record is processed when its first field is nonempty.
readLoopOk :: String -> [String]
readLoopOk s = ls ++ maybe [] final f
  where
    (ls, f) = records s
    final l = if null (fst (read2 l)) then [] else [l]

-- | Lines as seen by grep, sort, and other line utilities.
grepLines :: String -> [String]
grepLines = lines

-- | @${s##*c}@
afterLast :: Char -> String -> String
afterLast c = reverse . takeWhile (/= c) . reverse

-- | @${s%c*}@
beforeLast :: Char -> String -> String
beforeLast c s = case break (== c) (reverse s) of
  (_, [])    -> s
  (_, _ : r) -> reverse r

-- | @${s#*c}@
afterFirst :: Char -> String -> String
afterFirst c s = case break (== c) s of
  (_, [])    -> s
  (_, _ : r) -> r

-- | @${s%%c*}@
beforeFirst :: Char -> String -> String
beforeFirst c = takeWhile (/= c)

-- | @${s##*[cs]}@
afterLastAny :: [Char] -> String -> String
afterLastAny cs = reverse . takeWhile (`notElem` cs) . reverse

-- | @${s%[cs]*}@
beforeLastAny :: [Char] -> String -> String
beforeLastAny cs s = case break (`elem` cs) (reverse s) of
  (_, [])    -> s
  (_, _ : r) -> reverse r

-- | @${s%"${s##*[!c]}"}@: remove every trailing @c@.
stripTrailing :: Char -> String -> String
stripTrailing c = reverse . dropWhile (== c) . reverse

-- | @${s#c}@
dropOneLeading :: Char -> String -> String
dropOneLeading c (x : xs) | x == c = xs
dropOneLeading _ s = s

-- | @${s%c}@
dropOneTrailing :: Char -> String -> String
dropOneTrailing c s = case reverse s of
  (x : xs) | x == c -> reverse xs
  _ -> s

-- | @${s#"p"}@
stripPrefixOr :: String -> String -> String
stripPrefixOr p s = if p `isPrefixOf` s then drop (length p) s else s

-- | The reference @fnr@ helper: repeatedly replace the last occurrence of
-- @old@ until none remains.
replaceAll :: String -> String -> String -> String
replaceAll old new s
  | null old = s
  | otherwise = case lastSplit s of
      Nothing      -> s
      Just (a, b)  -> replaceAll old new (a ++ new ++ b)
  where
    lastSplit str = go (length str - length old)
      where
        go i | i < 0 = Nothing
             | old `isPrefixOf` drop i str = Just (take i str, drop (i + length old) str)
             | otherwise = go (i - 1)

replaceChar :: Char -> Char -> String -> String
replaceChar a b = map (\c -> if c == a then b else c)

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])    -> [a]
  (a, _ : r) -> a : splitOn c r

-- | Field splitting with @IFS=:@ where empty fields are irrelevant.
splitColon :: String -> [String]
splitColon = filter (not . null) . splitOn ':'

-- | Return the text following the first occurrence of @needle@.
findSub :: String -> String -> Maybe String
findSub needle = go
  where
    go hay | needle `isPrefixOf` hay = Just (drop (length needle) hay)
    go [] = Nothing
    go (_ : hs) = go hs

-- | True when the needles occur in order without overlapping, which is the
-- semantics of the case pattern @*"a"*"b"*...@.
inOrder :: [String] -> String -> Bool
inOrder [] _ = True
inOrder (n : ns) s = maybe False (inOrder ns) (findSub n s)

isPrefixOf', isSuffixOf' :: String -> String -> Bool
isPrefixOf' = isPrefixOf
isSuffixOf' = isSuffixOf

-- | A small immutable string set for exact line membership tests.
data Set = Tip | Bin Set String Set

mkSet :: [String] -> Set
mkSet xs = build (uniq (sort xs))
  where
    uniq (a : b : r) | a == b = uniq (b : r)
    uniq (a : r) = a : uniq r
    uniq [] = []
    build [] = Tip
    build ys = case splitAt (length ys `div` 2) ys of
      (l, m : r) -> Bin (build l) m (build r)
      (l, []) -> build l

member :: String -> Set -> Bool
member _ Tip = False
member x (Bin l m r) = case compare x m of
  LT -> member x l
  EQ -> True
  GT -> member x r
