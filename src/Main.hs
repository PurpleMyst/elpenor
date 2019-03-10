module Main where

import Compiler
import Marshal

import System.IO
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
  h <- openBinaryFile "compiled.dat" WriteMode
  B.hPut h (marshalWithType $ compile "print(\"hello, world\")")
  hClose h
