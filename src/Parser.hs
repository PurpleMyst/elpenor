{-# LANGUAGE ViewPatterns, LambdaCase, MultiWayIf #-}
module Parser where

import Prelude hiding (fail)
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.State.Lazy hiding (fail)
import Control.Monad.Except
import Data.Char
import Data.Foldable
import Data.Functor

import AST
import qualified Zipper as Z
import Zipper (Zipper)

type ZString = Zipper Char
type ErrorMsg = String

-- Parser type {{{
type Parser a = StateT ZString (Except ErrorMsg) a

anyChar :: Parser Char
anyChar = do
    z <- get
    case Z.maybeCurrent z of
      Just c  -> put (Z.zipRight z) >> return c
      Nothing -> throwError "Expected char, found EOF"

char :: Char -> Parser Char
char c = do
    c' <- anyChar
    if c == c'
      then return c
      else throwError $ "Expected '" ++ show c ++ ", found '" ++ show c' ++ "'"

word :: Parser String
word = Parser.takeWhile (not . isSpace)

token :: String -> Parser String
token s = do
  w <- word
  when (w /= s) (throwError ("Expected token \"" ++ s ++ "\", found \"" ++ w ++ "\""))
  return w

takeWhile :: (Char -> Bool) -> Parser String
takeWhile f = go
  where
    go = do
      z <- get
      let c = Z.current z
      if | null (Z.right z) -> return []
         | f c              -> do
           put (Z.zipRight z)
           (c :) <$> go
         | otherwise        -> return []

whitespace :: Parser String
whitespace = Parser.takeWhile isSpace

ignoreWs :: Parser a -> Parser a
ignoreWs p = whitespace *> p <* whitespace

-- }}}

-- elpenor parser {{{
identifier :: Parser AST
identifier = do
    w <- Parser.takeWhile isAlpha
    when (null w) (throwError "Expected non-empty identifier")
    return $ Identifier w

number :: Parser AST
number = do
    n <- Parser.takeWhile isDigit
    when (null n) (throwError "Expected non-empty number")
    return $ Number (read n)

string :: Parser AST
string = char '"' *> fmap AST.String (parseContents False)
  where
    parseContents False =
        (char '"'  $> []) <|>
        (char '\\' *> parseContents True) <|>
        parseContents True

    parseContents True =
        fmap (:) anyChar <*> parseContents False

functionCall :: Parser AST
functionCall = do
    fname <- ignoreWs identifier

    ignoreWs $ char '('

    FunctionCall fname <$> args
    -- char ')' is implicit in args
  where
    args = do
        arg <- ignoreWs expression
        ([arg] <$ char ')') <|> (char ',' >> ((arg : ) <$> args))

expression :: Parser AST
expression = asum [functionCall, identifier, number, string]

assignment :: Parser AST
assignment = do
    ignoreWs (token "let")

    lhs <- (\(Identifier s) -> s) <$> ignoreWs identifier

    ignoreWs (char '=')

    rhs <- ignoreWs expression

    return $ Assignment lhs rhs

statement :: Parser AST
statement = asum [assignment, functionCall] <* char ';'

program :: Parser AST
program = Program <$> many statement
-- }}}

parse :: String -> AST
parse (Z.fromList -> s) =
  case runExcept $ runStateT (ignoreWs program) s of
    Left e         -> error e
    Right (ast, z) -> if null (Z.right z)
                         then ast
                         else error $ "parse: could not parse remaining " ++ show (Z.right z)

-- vim: foldmethod=marker
