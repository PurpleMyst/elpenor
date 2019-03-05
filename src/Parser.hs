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

instance Applicative Parser where
  pure x = Parser $ \z -> Right (x, z)
  (Parser ff) <*> (Parser fa) = Parser $ \z -> case ff z of
      Left e        -> Left e
      Right (f, z') -> case fa z' of
        Left e         -> Left e
        Right (a, z'') -> Right (f a, z'')

instance Monad Parser where
  (Parser f) >>= g = Parser $ \z -> case f z of
      Left e        -> Left e
      Right (x, z')  -> let (Parser f') = g x in f' z'

  fail s = Parser $ \_ -> Left s

char :: Char -> Parser ()
char c = Parser $ \z -> if Z.current z == c 
                        then Right ((), Z.zipRight z) 
                        else Left $ "Expected '" ++ [c] ++ "' at index " ++ (show (Z.getOffset z)) ++ ", found '" ++ [Z.current z] ++ "'"

runParser :: Parser a -> String -> Either ErrorMsg (a, String)
runParser (Parser f) s = (fmap . fmap) Z.toList (f $ Z.fromList s)

parse :: String -> AST
parse = const AST
