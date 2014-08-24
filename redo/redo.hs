{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import           Control.Monad        (filterM, liftM, unless, guard)
import           Control.Exception    (catch, catchJust, IOException(..))
import           Data.Maybe           (listToMaybe)
import           Data.Typeable        (typeOf)
import           Data.Map.Lazy        (insert, fromList, toList, adjust)
import           Data.Digest.Pure.MD5 (md5)
import           System.Process       (createProcess, waitForProcess, shell,
                                       CreateProcess(..), StdStream(..), CmdSpec(..))
import           System.Environment   (getArgs, getEnvironment)
import           System.Exit          (ExitCode(..))
import           System.FilePath      (replaceBaseName, hasExtension, takeBaseName)
import           System.Directory     (renameFile, removeFile, doesFileExist,
                                       getDirectoryContents, createDirectoryIfMissing, removeDirectoryRecursive)
import           System.IO            (hPutStrLn, stderr, withFile, hGetLine, IOMode(..))
import           System.IO.Error      (ioeGetErrorType, isDoesNotExistError)
import           GHC.IO.Exception     (IOErrorType(..))
import qualified Data.ByteString.Lazy as BL

import           Debug.Trace          (trace)

traceShow' t = trace (show t) t

main :: IO ()
main =  mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target = do
  upToDate'<- upToDate target metaDepsDir
  unless upToDate' $  maybe printMissing redo' =<< redoPath target
  where redo'        :: FilePath -> IO ()
        redo' path   = do
          catchJust (\e -> guard $ isDoesNotExistError e)
            (removeDirectoryRecursive metaDepsDir)
            (\_ ->  return ()) 
          createDirectoryIfMissing True metaDepsDir
          writeFile (metaDepsDir ++ "/" ++ path) =<< md5' path
          oldEnv <- getEnvironment
          let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
          (_,_,_,ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
          exit <- waitForProcess ph
          case exit of
           ExitSuccess -> do renameFile tmp target
           ExitFailure code -> do hPutStrLn stderr $ "redo.do exited with error: " ++ show code
                                  removeFile tmp
        tmp          = target ++ "----redoing"
        metaDepsDir = ".redo/" ++ target
        printMissing = error $ "No .do file '" ++ target ++ "'"
        cmd path     = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
    where candidates = [target ++ ".do"] ++ if hasExtension target
                                            then [replaceBaseName target "default" ++ ".do"]
                                            else []

upToDate :: String -> FilePath -> IO Bool
upToDate target depDir = Control.Exception.catch
                  (do deps <- getDirectoryContents depDir
                      (traceShow' . all id) `liftM` mapM depUpToDate deps)
                  (\ (e::IOException) -> return False)
    where
--      depDir = ".redo/" ++ target
      depUpToDate :: FilePath -> IO Bool
      depUpToDate dep = Control.Exception.catch
                        (do oldMD5 <- withFile (depDir ++ "/" ++ dep) ReadMode hGetLine
                            newMD5 <- md5' dep
                            return $ oldMD5 == newMD5)
                        (\e -> return (ioeGetErrorType e == InappropriateType))


md5' :: FilePath -> IO String
md5' path = (show . md5) `liftM` BL.readFile path 
