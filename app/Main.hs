module Main where

import Interpreter
import Data.List
import System.Environment(getArgs)

main :: IO ()
main = do args <- getArgs
          let unusedVars = verify (program args)
          case unusedVars of
           [] -> run (program args)
           _  -> putStrLn $ "Unused Variables: " ++  (intercalate ", " unusedVars)

program :: [String] -> Program
program ["fail"] = failingProgram
program _        = testProgram -- programFromText $ readFile "test.input"
