module Config where

import           System.Directory (getCurrentDirectory)
import           System.IO.Error  (tryIOError)

-- Read from file, first line is username, second line is password
readCredential :: IO (String, String)
readCredential = do
    credentials <- tryIOError $ readFile "credentials/.credential"
    case credentials of
        Left _ -> do
            cwd <- getCurrentDirectory
            error $ "Attempting to read credentials/.credential file from working directory: " ++ cwd
        Right contents -> case lines contents of
            username : password : _ -> pure (username, password)
            _ -> error "Expected 2 lines in a file named 'credentials/.credential'. First line username, second line password."
