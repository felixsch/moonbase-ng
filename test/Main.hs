module Main where

import           System.Exit (exitFailure, exitSuccess)
import           Test.HUnit


main :: IO Int
main = putStrLn "***Test***" >> exitSuccess
