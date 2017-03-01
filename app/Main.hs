
module Main where

import           Absol.Utilities (outputToken)
import qualified Absol.Metaparse as P
import           Cmdargs
import           System.Exit
import           System.IO
import           System.IO.Error
import qualified Data.Text.IO as TI

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
        process = acquireMetaspecFile file (processMetaspecFile opts)
        processFailure ex = do
            hPrint stderr ex
            exitFailure :: IO ()
        processSuccess = do
            putStrLn $ outputToken ++ "Input file successfully processed."
            exitSuccess :: IO ()
runMetacompiler CLIOptions{cleanFlag=True} = undefined
-- TODO cleanup functionality

-- | Loads the metaspec file and executes the metacompiler processing on it.
acquireMetaspecFile :: FilePath -> (Handle -> IO a) -> IO a
acquireMetaspecFile file = withFile file ReadMode

-- | The main processing chain for the metacompiler.
processMetaspecFile :: CLIOptions -> Handle -> IO ()
processMetaspecFile _ mFile = TI.putStrLn =<< TI.hGetContents mFile
