module Main where

import Interpreter

programFromText :: String -> Program
programFromText s = read s

main :: IO ()
main = do
         f <- readFile "../test.input"
         run $ programFromText f
