module Main where

import Interpreter
import Data.List
import System.Environment(getArgs)

main :: IO ()
main = do args <- getArgs
          file <- readFile $ args !! 0
          let program = programFromText file
          let unusedVars = verify program
          case unusedVars of
           [] -> run program
           _  -> putStrLn $ "Unused Variables: " ++  (intercalate ", " unusedVars)
