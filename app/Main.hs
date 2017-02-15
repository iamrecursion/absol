
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Absol as A
-- import qualified Absol.Metalex as L
-- import qualified Absol.Metagen as G
-- import qualified Absol.Metaparse as P
-- import qualified Absol.Metaverify as V
import qualified Data.Text.IO as TI
import qualified Data.Text as T

-- import Cmdargs

import Options.Applicative 

-- Options
-- FILENAME : The name of the input metaspec file
-- [-r | --report] : Print a report on the safety of the language, but do not
-- generate any files.
-- [-c | --clean] : Remove any generated files (known from the name generation).
-- [-h | --help] : Print the help text.
-- [-n | --name NAME] : Override the name string contained in the file.
-- [-v | --version VERSION] : Override the version string contained in the file.
-- [-l | --log-verbose] : Set verbose logging mode.
-- [--log-file FILE] : Sets a log file. Otherwise defaults to stderr.

-- https://hackage.haskell.org/package/filepath-1.4.1.2/docs/System-FilePath-Posix.html

type CmdLineFlag = Bool
type File = FilePath
type CmdString = Maybe String

data CLIOptions = CLIOptions {
    filename :: FilePath,
    reportFlag :: CmdLineFlag,
    cleanFlag :: CmdLineFlag,
    langName :: CmdString,
    langVersion :: CmdString,
    verboseFlag :: CmdLineFlag,
    logFile :: Maybe FilePath
} deriving (Eq, Show)

parseLangVersion :: Parser CmdString
parseLangVersion = optional $ option str (
        short 'v' <>
        long "version" <>
        help "Use the provided version instead of the one embedded \
            \in the file." <>
        metavar "STRING"
    )

parseVerboseFlag :: Parser CmdLineFlag
parseVerboseFlag = switch (
        short 'l' <>
        long "log-verbose" <>
        help "Enable verbose logging."
    )

parseLogFile :: Parser (Maybe FilePath) 
parseLogFile = optional $ option str (
        long "log-file" <>
        help "Log to the provided FILE." <>
        metavar "FILE"
    )

parseLangName :: Parser CmdString
parseLangName = optional $ option str (
        short 'n' <>
        long "name" <>
        help "Use the provided name instead of the one embedded in the file." <>
        metavar "STRING"
    )

parseReportFlag :: Parser CmdLineFlag
parseReportFlag = switch (
        short 'r' <>
        long "report" <>
        help "Only print a report on the correctness of the input file."
    )
 
parseFilename :: Parser FilePath
parseFilename = argument str (metavar "FILE")

parseCleanFlag :: Parser CmdLineFlag
parseCleanFlag = switch (
        short 'c' <>
        long "clean" <>
        help "Delete metacompiler products. \
        \   Products will be deleted by language name and version. This data \
        \   can be provided by the --name and --version flags, or the input \
        \   file "
    )

parseCLIOptions :: Parser CLIOptions
parseCLIOptions = CLIOptions
    <$> parseFilename
    <*> parseReportFlag
    <*> parseCleanFlag
    <*> parseLangName
    <*> parseLangVersion
    <*> parseVerboseFlag
    <*> parseLogFile

-- Can I make this more flexible? Yeah, but no longer infix. Cool! TODO
withParserInfo :: Parser a -> String -> ParserInfo a
withParserInfo options description =
    info (helper <*> options) $ progDesc description

main :: IO ()
main = runMetacompiler =<< execParser (
        parseCLIOptions `withParserInfo` "Run the ABSOL Metacompiler."
    )

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
