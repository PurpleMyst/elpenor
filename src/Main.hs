module Main where

import Compiler
import Marshal

import System.IO
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    h <- openBinaryFile "compiled.dat" WriteMode
    --print (co_code co)
    B.hPut h (marshalWithType co)
    hClose h
  where
    co = compile "let x = 3; print(x);"
