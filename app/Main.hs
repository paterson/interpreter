module Main where

import Interpreter
import Data.List
import System.Environment(getArgs)

main :: IO ()
main = do args <- getArgs
          case unusedVars of
           [] -> run program
           _  -> putStrLn $ "Unused Variables: " ++  (intercalate ", " unusedVars)

program :: Program
program = do args <- getArgs
             putStrLn $ show args
             return (case head args of
                      "fail" -> failingProgram
                      _      -> testProgram)     -- programFromText $ readFile "test.input"

unusedVars = verify program
