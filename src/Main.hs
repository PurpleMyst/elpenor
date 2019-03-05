module Main where

import Parser

main :: IO ()
main = print $ parse "const x = 2;"
