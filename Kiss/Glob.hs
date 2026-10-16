-- | POSIX pattern matching (case patterns) and pathname expansion.
--
-- The reference package manager resolves packages, archives, and database
-- entries with shell globs, so lookup order and the "unmatched pattern stays
-- literal" behaviour are part of the observable protocol.
module Kiss.Glob (globMatch, hasMagic, escapeGlob, globIn, glob) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isUpper, isSpace, isPunctuation, isSymbol, isControl, isHexDigit, isPrint)
import Data.List (sort)
import Kiss.FS (getEntries, isDir, lexists)

-- | Match a string against a shell pattern.  @/@ is not special: callers
-- performing pathname expansion match one component at a time.
globMatch :: String -> String -> Bool
globMatch [] s = null s
globMatch ('*' : ps) s = any (globMatch ps') (tails' s)
  where ps' = dropWhile (== '*') ps
globMatch ('?' : ps) (_ : cs) = globMatch ps cs
globMatch ('?' : _) [] = False
globMatch ('[' : ps) s = case parseBracket ps of
  Just (ok, rest) -> case s of
    (c : cs) -> ok c && globMatch rest cs
    []       -> False
  Nothing -> case s of
    ('[' : cs) -> globMatch ps cs
    _          -> False
globMatch ('\\' : p : ps) (c : cs) = p == c && globMatch ps cs
globMatch ('\\' : p : _) [] = const False p
globMatch (p : ps) (c : cs) = p == c && globMatch ps cs
globMatch (_ : _) [] = False

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs@(_ : r) = xs : tails' r

-- | Parse a bracket expression following @[@.
parseBracket :: String -> Maybe (Char -> Bool, String)
parseBracket s0 = start s0
  where
    start ('!' : r) = fmap neg (items True r [])
    start ('^' : r) = fmap neg (items True r [])
    start r = items True r []
    neg (f, r) = (not . f, r)

    items :: Bool -> String -> [Char -> Bool] -> Maybe (Char -> Bool, String)
    items _ [] _ = Nothing
    items False (']' : r) acc = Just (\c -> any ($ c) acc, r)
    items _ ('[' : ':' : r) acc = case breakClass r of
      Just (name, rest) -> items False rest (cls name : acc)
      Nothing -> items False (':' : r) ((== '[') : acc)
    items _ s acc = let (c, r) = one s in case r of
      ('-' : r2) | not (null r2) && take 1 r2 /= "]" ->
        let (d, r3) = one r2 in items False r3 ((\x -> x >= c && x <= d) : acc)
      _ -> items False r ((== c) : acc)

    one ('\\' : c : r) = (c, r)
    one (c : r) = (c, r)
    one [] = (' ', [])

    breakClass r = case break (== ':') r of
      (name, ':' : ']' : rest) | all isAlpha name -> Just (name, rest)
      _ -> Nothing

    cls :: String -> Char -> Bool
    cls n = case n of
      "alpha" -> isAlpha
      "digit" -> isDigit
      "alnum" -> isAlphaNum
      "upper" -> isUpper
      "lower" -> isLower
      "space" -> isSpace
      "blank" -> \c -> c == ' ' || c == '\t'
      "punct" -> \c -> isPunctuation c || isSymbol c
      "cntrl" -> isControl
      "xdigit" -> isHexDigit
      "print" -> isPrint
      "graph" -> \c -> isPrint c && c /= ' '
      _ -> const False

-- | Whether a pattern contains an unescaped pattern metacharacter.
hasMagic :: String -> Bool
hasMagic ('\\' : _ : r) = hasMagic r
hasMagic (c : r) = c `elem` "*?[" || hasMagic r
hasMagic [] = False

escapeGlob :: String -> String
escapeGlob = concatMap (\c -> if c `elem` "*?[\\" then ['\\', c] else [c])

unescape :: String -> String
unescape ('\\' : c : r) = c : unescape r
unescape (c : r) = c : unescape r
unescape [] = []

-- | Expand @"literal"pattern@, returning the sorted matches or, as the shell
-- does without @nullglob@, the literal word when nothing matches.
globIn :: String -> String -> IO [String]
globIn prefix pat = do
  let full = escapeGlob prefix ++ pat
  if not (hasMagic pat)
    then return [prefix ++ pat]
    else do
      ms <- expand full
      return (if null ms then [prefix ++ pat] else ms)

-- | Expand an unquoted pattern word.
glob :: String -> IO [String]
glob = globIn ""

expand :: String -> IO [String]
expand pat = do
  rs <- go "" (splitComponents pat)
  ex <- mapM lexists rs
  return (sort [r | (r, True) <- zip rs ex])
  where
    go pre [c] = matchComponent pre c (return . (: []))
    go pre (c : cs) = matchComponent pre c (\p -> do
      d <- if hasMagic c then isDir p else return True
      if d then go (p ++ "/") cs else return [])
    go pre [] = return [pre]

    matchComponent pre c k
      | hasMagic c = do
          names <- getEntries (if null pre then "." else pre)
          let hidden n = take 1 n == "."
              dotOk = take 1 (unescape' c) == "."
              ok n = n /= "." && n /= ".." && (not (hidden n) || dotOk) && globMatch c n
          fmap concat (mapM (\n -> k (pre ++ n)) (sort (filter ok names)))
      | otherwise = k (pre ++ unescape c)

    unescape' ('\\' : c : r) = c : r
    unescape' s = s

splitComponents :: String -> [String]
splitComponents s = case break (== '/') s of
  (a, [])    -> [a]
  (a, _ : r) -> a : splitComponents r
