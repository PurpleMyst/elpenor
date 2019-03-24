{-# LANGUAGE OverloadedStrings #-}
module Main where

import Compiler
import CodeObject
import Marshal

import System.IO
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B

writeCodeObject :: String -> CodeObject -> IO ()
writeCodeObject fname co = do
  h <- openBinaryFile fname WriteMode
  -- FIXME: Calculate the magic number instead of hard-coding it.
  B.hPut h "B\r\r\n"
  -- XXX: Do these actually mean anything? `run_pyc_file` ignores them
  B.hPut h "\0\0\0\0\0\0\0\0\0\0\0\0"
  B.hPut h $ marshalWithType co
  hClose h

main :: IO ()
main =
    getArgs >>= (readFile . head) >>= (writeCodeObject "elpenor.pyc" . compile)
