
module Main where

import           Cmdargs
import qualified Data.Text.IO as TI
import           System.Exit

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
runMetacompiler opts = do
    TI.putStrLn "Woop!"
    putStrLn $ "Filename " ++ filename opts
    putStrLn $ "Report " ++ show (reportFlag opts)
    putStrLn $ "Clean " ++ show (cleanFlag opts)
    putStrLn $ "LangName "  ++ show (langName opts)
    putStrLn $ "LangVer " ++ show (langVersion opts)
    putStrLn $ "Verbose " ++ show (verboseFlag opts)
    putStrLn $ "LogFile " ++ show (logFile opts)
    exitSuccess
