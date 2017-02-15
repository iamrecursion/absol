{-|
Module      : Cmdargs
Description : Functionality for parsing the command-line arguments of the ABSOL
              metacompiler.
Copyright   : (c) Ara Adkins, 2017
License     : Closed
Maintainer  : me@ara.io
Stability   : experimental
Portability : GHC

This module provides functionality for parsing the metacompiler command-line 
arguments, as well as the data-type for storing them. 
-}
module Cmdargs
    (
        CLIOptions(..),
        parseCLIOptions,
        withParserInfo,
        Options.Applicative.execParser
    ) where

import           Options.Applicative

type CmdLineFlag = Bool
type File = FilePath
type CmdString = Maybe String

-- | Storage for the command-line configuration.
--
-- This type is filled with the parsed options at program start.
data CLIOptions = CLIOptions {
    filename    :: File,
    reportFlag  :: CmdLineFlag,
    cleanFlag   :: CmdLineFlag,
    langName    :: CmdString,
    langVersion :: CmdString,
    verboseFlag :: CmdLineFlag,
    logFile     :: Maybe File
} deriving (Eq, Show)

-- | A utility function for specifying the program description for the CLI help.
-- 
-- It is given a description and the program header text respectively.
withParserInfo :: Parser a -> String -> String -> ParserInfo a
withParserInfo options description headText =
    info (helper <*> options) $
    fullDesc <> progDesc description <> header headText

-- | Parses the program's command-line options.
parseCLIOptions :: Parser CLIOptions
parseCLIOptions = CLIOptions
    <$> parseFilename
    <*> parseReportFlag
    <*> parseCleanFlag
    <*> parseLangName
    <*> parseLangVersion
    <*> parseVerboseFlag
    <*> parseLogFile

-- | Parses the input filepath from the CLI arguments.
-- 
-- This path specifies the metaspec file on which the metacompiler will operate.
parseFilename :: Parser FilePath
parseFilename = argument str (
        metavar "FILENAME" <>
        help "The metaspec file to run the metacomplier on. "
    )

-- | Parses the reporting flag from the CLI flags.
-- 
-- If this flag is set, the program will analyse the input file for semantic 
-- correctness and then exit, without performing any codegen. 
parseReportFlag :: Parser CmdLineFlag
parseReportFlag = switch (
        short 'r' <>
        long "report" <>
        help "Only print a report on the correctness of the input file."
    )

-- | Parses the cleanup flag from the CLI arguments.
-- 
-- If specified, the program will delete all generated files. These files are
-- determined from the language name and version.
parseCleanFlag :: Parser CmdLineFlag
parseCleanFlag = switch (
        short 'c' <>
        long "clean" <>
        help "Delete metacompiler output. \
        \ Output will be deleted by language name and version. This data\
        \ can be provided by the --name and --version flags, or the input\
        \ file "
    )

-- | Parses the language name string from the CLI flags.
--
-- This name string is used to override the version specified in the input file.
parseLangName :: Parser CmdString
parseLangName = optional $ option str (
        short 'n' <>
        long "name" <>
        help "Use the provided name STRING instead of the one embedded in\
            \ the file." <>
        metavar "STRING"
    )

-- | Parses the language version string from the CLI flags.
--
-- This version string is used to override the version specified in the input
-- file.
parseLangVersion :: Parser CmdString
parseLangVersion = optional $ option str (
        short 'v' <>
        long "version" <>
        help "Use the provided version STRING instead of the one embedded in\
            \ the file." <>
        metavar "STRING"
    )

-- | Parses the verbosity flag from the CLI flags.
--
-- This flag sets whether the program will log verbose output.
parseVerboseFlag :: Parser CmdLineFlag
parseVerboseFlag = switch (
        short 'l' <>
        long "log-verbose" <>
        help "Enable verbose logging."
    )

-- | Parses the log file path from the CLI flags.
--
-- If this is set, the program will log to the specified file instead of stdout/
-- stderr.
parseLogFile :: Parser (Maybe FilePath)
parseLogFile = optional $ option str (
        long "log-file" <>
        help "Log to the provided FILE." <>
        metavar "FILE"
    )
