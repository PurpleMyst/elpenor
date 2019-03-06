module AST where

data AST = Identifier String
         | Number Integer
         | String String
         | Assignment AST AST
         | FunctionCall AST [AST]
         | Program [AST]
         deriving (Show, Eq)
