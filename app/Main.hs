module Main where

import Interpreter

main :: IO ()
main = do
         f <- readFile "test.input"
         putStrLn $ show $ snd testProgram
         --run $ programFromText f
         run testProgram
