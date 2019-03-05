module Parser(parse) where

import Control.Monad.State.Lazy
import Data.Char (isSpace)

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


takeWhile :: (Char -> Bool) -> Parser String
takeWhile f = Parser go
  where
    go z
        | null (Z.right z) = Right ([], z)
        | f c            = let (Right (s, z')) = go (Z.zipRight z) in Right (c : s, z')
        | otherwise      = Right ([], z)
      where
        c = Z.current z

takeUntil :: (Char -> Bool) -> Parser String
takeUntil = Parser.takeWhile . fmap not

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = Parser go
  where
    go z
        | null (Z.right z) = Right ((), z)
        | f c              = go (Z.zipRight z)
        | otherwise        = Right ((), z)
      where
        c = Z.current z

skipUntil :: (Char -> Bool) -> Parser ()
skipUntil = Parser.skipWhile . fmap not

runParser :: Parser a -> String -> Either ErrorMsg (a, ZString)
runParser (Parser f) s = f (Z.fromList s)

-- example parser
addition :: Parser Integer
addition = do
  a <- read <$> takeUntil isSpace
  skipWhile isSpace
  char '+'
  skipWhile isSpace
  b <- read <$> takeUntil isSpace
  return (a + b)

parse :: String -> AST
parse = const AST
