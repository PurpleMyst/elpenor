module Compiler(compile) where

import Control.Monad.State.Lazy

import AST

data Compiler = Compiler
              { names     :: [String]
              , constants :: [AST]
              , code      :: String
              } deriving (Eq, Show)

compile' :: AST -> State Compiler ()
compile' = undefined

compile :: AST -> Compiler
compile (Program xs) = undefined
