module Main where

import Interpreter

main :: IO ()
main = do
         f <- readFile "test.input"
         putStrLn $ show testProgram
         --run $ programFromText f
         run testProgram
