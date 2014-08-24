{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad        (filterM, liftM, unless, guard)
import           Control.Exception    (catch, catchJust, IOException)
import           Data.Maybe           (listToMaybe)
-- import           Data.Typeable        (typeOf)
import           Data.Map.Lazy        (insert, fromList, toList, adjust)
import           Data.Digest.Pure.MD5 (md5)
import           System.Process       (createProcess, waitForProcess, shell, CreateProcess(..))
import           System.Environment   (getArgs, getEnvironment, getProgName)
import  System.Posix.Env (getEnv)
import           System.Exit          (ExitCode(..))
import           System.FilePath      (replaceBaseName, hasExtension, takeBaseName, (</>))
import           System.Directory     (renameFile, removeFile, doesFileExist,
                                       getDirectoryContents, createDirectoryIfMissing, removeDirectoryRecursive)
import           System.IO            (hPutStrLn, stderr, withFile, hGetLine, IOMode(..))
import           System.IO.Error      (ioeGetErrorType, isDoesNotExistError)
import           GHC.IO.Exception     (IOErrorType(..))

import qualified Data.ByteString.Lazy as BL
import           Debug.Trace          (trace)

traceShow' :: Show a => a -> a
traceShow' t = trace (show t) t

md5' :: FilePath -> IO String
md5' path = (show . md5) `liftM` BL.readFile path 

metaDir :: String
metaDir = ".redo"

main :: IO ()
main = do
       mapM_ redo =<< getArgs
       progName   <- getProgName
       redoTarget <- getEnv "REDO_TARGET"
       case (progName, redoTarget) of
        ("redo-ifchanged", Just target) ->
          mapM_ (writeMD5 target) =<< getArgs
        ("redo-ifchanged", Nothing) -> error "Missing REDO_TARGET env"
        _ -> return ()
  where writeMD5 :: FilePath -> FilePath -> IO ()
        writeMD5 redoTarget dep = writeFile (metaDir </> redoTarget </> dep ) =<< md5' dep

redo :: String -> IO ()
redo target = do
  upToDate'<- upToDate target
  unless upToDate' $  maybe printMissingDo redo' =<< redoPath target
  where redo'        :: FilePath -> IO ()
        redo' path   = do
          catchJust (guard . isDoesNotExistError)
            (removeDirectoryRecursive metaDepsDir)
            (\_ ->  return ()) 
          createDirectoryIfMissing True metaDepsDir
          writeFile (metaDepsDir </> path) =<< md5' path
          oldEnv <- getEnvironment
          let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
          (_,_,_,ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
          exit <- waitForProcess ph
          case exit of
           ExitSuccess -> renameFile tmp target
           ExitFailure code -> do hPutStrLn stderr $ "redo.do exited with error: " ++ show code
                                  removeFile tmp
        tmp          = target ++ "----redoing"
        metaDepsDir = metaDir </> target
        printMissingDo = do
          exist <- doesFileExist target
          unless exist $ error $ "No .do file '" ++ target ++ "'"
        cmd path     = unwords ["sh -x", path, "0", takeBaseName target, tmp, ">", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
    where candidates = (target ++ ".do") : [replaceBaseName target "default" ++ ".do" | hasExtension target]

upToDate :: FilePath -> IO Bool
upToDate target = Control.Exception.catch
                  (do exists <- doesFileExist target
                      if exists
                        then do deps <- getDirectoryContents (metaDir </> target)
                                (traceShow' . and) `liftM` mapM depUpToDate deps
                        else return False)
                  (\ (_::IOException) -> return False)
  where depUpToDate :: FilePath -> IO Bool 
        depUpToDate dep = Control.Exception.catch
                          (do oldMD5 <- withFile (metaDir </> target </> dep) ReadMode hGetLine
                              newMD5 <- md5' dep
                              doScript <- redoPath dep
                              case doScript of
                               Nothing -> return $ oldMD5 == newMD5
                               Just _ -> do upToDate' <- upToDate dep
                                            return $ (oldMD5 == newMD5) && upToDate')                          
                          (\e -> return (ioeGetErrorType e == InappropriateType))
