{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Absol as A
import qualified Absol.Metalex as L
import qualified Absol.Metagen as G
import qualified Absol.Metaparse as P
import qualified Absol.Metaverify as V
import qualified Data.Text.IO as T

main :: IO ()
main = do
    L.someFunc
    G.someFunc
    P.someFunc
    V.someFunc
    A.myFunc
    T.putStrLn "Woo!!"
    return ()
