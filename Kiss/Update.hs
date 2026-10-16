-- | Repository update and extension listing (kiss.el's kiss-update).
module Kiss.Update (pkgUpdate, pkgHelpExt) where

import Control.Monad (foldM, forM_, unless, when)
import Data.IORef
import System.Exit (ExitCode (..), exitWith)
import System.IO

import Kiss.Env
import Kiss.FS
import Kiss.Hook
import Kiss.OS
import Kiss.Search
import Kiss.Util

-- | @pkg_update@
pkgUpdate :: Env -> IO ()
pkgUpdate e = do
  log1 e "Updating repositories"
  path <- getVar "KISS_PATH"
  seen <- newIORef []
  forM_ (splitColon path) $ \repo0 -> do
    up <- withDevNull (\n -> spawn defSpec { sOut = UseHandle n } "git"
            ["-C", repo0, "rev-parse", "HEAD@{upstream}"])
    target <- if up == ExitSuccess
      then do
        (c1, subm) <- capture defSpec "git" ["-C", repo0, "rev-parse", "--show-superproject-working-tree"]
        check c1
        let sub = chomp subm
        (c2, top) <- capture defSpec "git" ["-C", if null sub then repo0 else sub, "rev-parse", "--show-toplevel"]
        check c2
        return (Just (chomp top, True))
      else do
        d <- isDir repo0
        return (if d then Just (repo0, False) else Nothing)
    case target of
      Nothing -> return ()
      Just (repo, isGit) -> updateRepo e seen repo isGit
  log1 e "Run 'kiss U' to upgrade packages"
  where
    chomp = reverse . dropWhile (== '\n') . reverse

-- | @pkg_update_repo@
updateRepo :: Env -> IORef [String] -> FilePath -> Bool -> IO ()
updateRepo e seen repo isGit = do
  okCd <- isDir repo
  unless okCd $ die e ("Repository '" ++ repo ++ "' inaccessible") ""
  cd repo
  pwd <- getPWD
  done <- readIORef seen
  unless (pwd `elem` done) $ do
    writeIORef seen (done ++ [pwd])
    log2 e pwd " "
    (own, user) <- amOwner e pwd
    unless own $ putStrLn ("Need \"" ++ user ++ "\" to update")
    runHook e ["pre-update", if own then "0" else "1", user]
    when isGit $ do
      (_, v) <- capture defSpec "git" ["config", "--get", "merge.verifySignatures"]
      when (takeWhile (/= '\n') v == "true") $ putStrLn "Signature verification enabled."
      let asOwner args = if own then run "git" args else asUser e user ("git" : args)
      asOwner ["pull"] >>= check
      asOwner ["submodule", "update", "--remote", "--init", "-f"] >>= check
    runHook e ["post-update"]
    motd <- readFileMaybe "MOTD"
    case motd of
      Nothing -> return ()
      Just m -> do
        log2 e pwd "Printing MOTD"
        putStr m

-- | @pkg_help_ext@
pkgHelpExt :: Env -> IO ()
pkgHelpExt e = do
  log1 e "Installed extensions (kiss-* in PATH)"
  path <- getVar "PATH"
  found <- pkgFindAll e "kiss-*" "-x" (Just path)
  when (null found) $ logK e "'kiss-*' not found" "" "ERROR"
  -- Duplicate names shift the head of the list, as the reference's loop
  -- does with "shift" while iterating its original arguments.
  (list, _, maxw) <- foldM (\(xs, names, m) p -> do
      let n = afterFirstStr "/kiss-" p
      if n `elem` names
        then return (drop 1 xs, names, m)
        else return (xs, names ++ [n], if length n > m then length n + 1 else m))
    (found, [], 0) found
  forM_ list $ \p -> do
    c <- fmap (maybe "" id) (readFileMaybe p)
    let second = case lines c of
          (_ : l : _) -> l
          _ -> ""
        cmt = helpComment second
        name = afterFirstStr "/kiss-" p
    hPutStrLn stderr (eC1 e ++ "->" ++ eC3 e ++ " " ++ name ++ replicate (maxw - length name) ' ' ++ " " ++ cmt)
  where
    afterFirstStr sep s = maybe s id (findSub sep s)
    -- IFS=\#$IFS read -r _ cmt: '#' is an extra field delimiter.
    helpComment l =
      let s0 = dropWhile isIfsWs l
          rest = dropWhile isIfsWs (dropWhile (\c -> c /= '#' && not (isIfsWs c)) s0)
          rest' = case rest of
            ('#' : t) -> dropWhile isIfsWs t
            _ -> rest
      in trimWs rest'
