
module Main where

-- import qualified Absol as A
-- import qualified Absol.Metalex as L
-- import qualified Absol.Metagen as G
-- import qualified Absol.Metaparse as P
-- import qualified Absol.Metaverify as V
import qualified Data.Text.IO as T

import Cmdargs

import Options.Applicative 

main :: IO ()
main = T.putStrLn "Hello, Jay!"
-- main = run =<< execParser
--     (parseCLIOptions `withParserInfo` "Run the ABSOL Metacompiler")

-- run :: Options -> IO ()
-- run (Options app cmd) = do
--     putStrLn app
--     case cmd of
--         Start url version -> do
--             putStrLn url
--             putStrLn version
--         Status build -> putStrLn build
--         Release build rApp -> do
--             putStrLn build
--             putStrLn rApp
