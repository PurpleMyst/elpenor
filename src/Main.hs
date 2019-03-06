module Main where

import Compiler (compile)

main :: IO ()
main = print $ compile "print(\"hello, world\")"
