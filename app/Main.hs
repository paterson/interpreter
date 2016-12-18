module Main where

import Interpreter

programFromText :: String -> Program
programFromText s = read s

main :: IO ()
main = run $ programFromText $ readFile "../test.input"
