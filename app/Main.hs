{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Metalex as L
import qualified Metagen as G
import qualified Metaparse as P
import qualified Metaverify as V
import qualified Data.Text.IO as T

main :: IO ()
main = do
    L.someFunc
    G.someFunc
    P.someFunc
    V.someFunc
    T.putStrLn "Woo!!"
    return ()
