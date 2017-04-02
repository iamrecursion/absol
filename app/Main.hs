module Main where

import           Absol.Utilities (outputToken)
import qualified Absol.Metaparse as P
import           Absol.Metaverify
import           Cmdargs
import qualified Data.Text.IO as TI
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error

-- | The main function for ABSOL.
main :: IO ()
main = runMetacompiler =<< execParser (
        withParserInfo
            parseCLIOptions
            "Run the absol metacompiler."
            "ABSOL :: Automatic Builder for Semantically Oriented Languages"
    )

-- | Contains the main execution context of the metacompiler.
runMetacompiler :: CLIOptions -> IO ()
runMetacompiler opts@CLIOptions{filename=file, cleanFlag=False} = do
    putStrLn $ outputToken ++ "Executing the ABSOL metacompiler on " ++ file 
    result <- tryIOError process
    case result of
        Left ex -> processFailure ex
        Right _ -> processSuccess
    return ()
    where
        process = acquireMetaspecFile file (processMetaspecFile opts file)
        processFailure ex = do
            hPrint stderr ex
            exitFailure :: IO ()
        processSuccess = exitSuccess :: IO ()
runMetacompiler CLIOptions{cleanFlag=True} = 
    putStrLn "Cleaning not yet implemented."

-- | Loads the metaspec file and executes the metacompiler processing on it.
acquireMetaspecFile :: FilePath -> (Handle -> IO a) -> IO a
acquireMetaspecFile file = withFile file ReadMode

-- | The main processing chain for the metacompiler.
processMetaspecFile :: CLIOptions -> String -> Handle -> IO ()
processMetaspecFile 
    CLIOptions{logFile = reportFile, outputDirectory = outDir, verboseFlag = v} 
    filename mFile = do
    contents <- TI.hGetContents mFile
    case P.parseMetaspecFile filename contents of
        Left err -> hPutStr stderr $ P.parseErrorPretty err
        Right (ast, _) -> do
            let (result, diagnostic) = verifyLanguage ast
            -- Handle user-defined output file / path.
            case reportFile of
                Just fileName -> do
                    dir <- case outDir of 
                        Just directory -> return directory
                        Nothing -> getCurrentDirectory
                    let outPath = dir </> fileName
                    withFile outPath WriteMode (writeLogFile diagnostic)
                Nothing -> return ()
            -- Print diagnostics based on logging preferences.
            if v then
                putStrLn diagnostic
            else
                putStrLn $ takeWhile (/= '\n') diagnostic -- get first line
            if result then
                putStrLn $ outputToken ++ "Success"
            else 
                putStrLn $ outputToken ++ "Failure"
            return ()
    where
        writeLogFile diag hndl = hPutStr hndl diag  
