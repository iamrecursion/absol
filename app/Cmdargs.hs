module Cmdargs
    -- (
    --     -- CLIOptions(..),
    --     Options(..),
    --     Command,
    --     App,
    --     Version,
    --     URL,
    --     BuildID,
    --     parseCLIOptions,
    --     withParserInfo
    -- ) where
    where

import           Options.Applicative

import           Data.Text

-- | Obtains parser information and help text from the generated parser function
--
-- It is applied to a Parser and is able to generate a basic help text from the
-- static definition of that parser.
withParserInfo :: Parser a -> String -> ParserInfo a
withParserInfo options description =
    info (helper <*> options) $ progDesc description

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

type SpecFile = FilePath
type ReportFlag = Bool
type CleanFlag = Bool
type VerboseFlag = Bool
type SpecName = Text
type SpecVersion = Text
type LogFile = FilePath

data CLIOptions = CLIOptions {
    inputFile :: SpecFile,
    reportCheck :: ReportFlag,
    cleanFiles :: CleanFlag,
    verboseReporting :: VerboseFlag,
    languageName :: SpecName,
    languageVersion :: SpecVersion,
    logFilePath :: LogFile
}  deriving (Eq, Show)

defaultCLIOptions :: CLIOptions
defaultCLIOptions = CLIOptions {
    inputFile = undefined,
    reportCheck = False,
    cleanFiles = False,
    verboseReporting = False,
    languageName = undefined,
    languageVersion = undefined,
    logFilePath = "./absol.log"
}

parseCLIOptions :: Parser CLIOptions
parseCLIOptions = undefined

parseInputFile :: Parser SpecFile
parseInputFile = argument str (metavar "INPUT-FILE")

-- type NameString = Text
-- type VersionString = Text
-- type CleanFlag = Bool
-- type VerboseFlag = Bool
-- type SpecPath = FilePath

-- -- | A type for containing the command-line options of the program.
-- data CLIOptions = CLIOptions {
--     path       :: SpecPath,
--     clean      :: CleanFlag,
--     name       :: NameString,
--     version    :: VersionString,
--     logVerbose :: VerboseFlag
-- } deriving (Eq, Show)

-- type App = String
-- type Version = String
-- type URL = String
-- type BuildID = String

-- data Command
--     = Start URL Version
--     | Status BuildID
--     | Release BuildID App

-- data Options = Options App Command

-- parseCLIOptions :: Parser Options
-- parseCLIOptions = Options <$> parseApp <*> parseCommand

-- parseApp :: Parser App
-- parseApp = strOption
--     $ short 'a'
--     <> long "app"
--     <> metavar "COMPILE-APP"
--     <> help "The Application to compile"

-- parseCommand :: Parser Command
-- parseCommand = subparser $
--     command "start" (parseStart `withParserInfo` "Start a build.") <>
--     command "status" (parseStatus `withParserInfo` "Check build status.") <>
--     command "release" (parseRelease `withParserInfo` "Release build.")

-- parseStart :: Parser Command
-- parseStart = Start
--     <$> argument str (metavar "SOURCE-URL")
--     <*> argument str (metavar "VERSION")

-- parseStatus :: Parser Command
-- parseStatus = Status
--     <$> argument str (metavar "BUILD-ID")

-- parseRelease :: Parser Command
-- parseRelease = Release
--     <$> argument str (metavar "BUILD-ID")
--     <*> argument str (metavar "RELEASE-APP")

-- | Parses the command-line options for the program.
-- parseCLIOptions :: Parser CLIOptions
-- parseCLIOptions = CLIOptions
--     <$> parsePath
--     <*> parseCleanFlag
--     <*> parseLanguageName
--     <*> parseLanguageVersion
--     <*> parseVerboseFlag

-- parsePath :: Parser SpecPath
-- parsePath = path
--     <$> argument str (metavar "FILEPATH")

-- parseCleanFlag :: Parser CleanFlag
-- parseCleanFlag = undefined

-- parseLanguageName :: Parser NameString
-- parseLanguageName = undefined

-- parseLanguageVersion :: Parser VersionString
-- parseLanguageVersion = undefined

-- parseVerboseFlag :: Parser VerboseFlag
-- parseVerboseFlag = undefined
