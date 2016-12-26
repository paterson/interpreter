module Main where

import Interpreter

main :: IO ()
main = do
         f <- readFile "test.input"
         --run $ programFromText f
         run testProgram
