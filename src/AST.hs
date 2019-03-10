module AST(AST(..)) where

import Data.Int

data AST = Identifier String
         | Number Int32
         | String String
         | Assignment String AST
         | FunctionCall AST [AST]
         | None
         | Program [AST]
         deriving (Show, Eq)
