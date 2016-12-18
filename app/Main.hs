module Main where

import Interpreter

main :: IO ()
main = do
         f <- readFile "test.input"
         --debug $ programFromText f
         run testProgram
