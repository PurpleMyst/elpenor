module Parser(parse) where

import Control.Monad.State.Lazy

import AST
import qualified Zipper as Z
import Zipper (Zipper)

type ZString = Zipper Char
type ErrorMsg = String
newtype Parser a = Parser (ZString -> Either ErrorMsg (a, ZString))

instance Functor Parser where
  fmap f (Parser g) = Parser $ \z -> case g z of
    Left e        -> Left e
    Right (x, z') -> Right ((f x), z')

runParser :: Parser a -> String -> Either ErrorMsg (a, String)
runParser (Parser f) s = fmap (fmap Z.toList) (f $ Z.fromList s)

parse :: String -> AST
parse = const AST
