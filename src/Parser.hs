module Parser(parse) where
-- FIXME: Better errors!!!

import Prelude hiding (fail)
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Char
import Data.Foldable
import Data.Functor

import AST
import qualified Zipper as Z
import Zipper (Zipper)

type ZString = Zipper Char
type ErrorMsg = String

-- Parser type {{{
newtype Parser a = Parser (ZString -> Either ErrorMsg (a, ZString))

-- XXX: We can probably minify these implementations.
instance Functor Parser where
  fmap f (Parser g) = Parser $ \z -> case g z of
    Left e        -> Left e
    Right (x, z') -> Right (f x, z')

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
      Right (x, z') -> let (Parser f') = g x in f' z'

instance MonadFail Parser where
  fail = Parser . const . Left

instance Alternative Parser where
  empty = fail ""
  (Parser f) <|> (Parser g) = Parser $ \z -> case f z of
    Left _        -> g z
    Right (x, z') -> Right (x, z')

anyChar :: Parser Char
anyChar = Parser $ \z -> Right (Z.current z, Z.zipRight z)

char :: Char -> Parser Char
char c = Parser $ \z -> if Z.maybeCurrent z == Just c
                        then Right (c, Z.zipRight z)
                        else Left $ "Expected '" ++ [c] ++ "' at index " ++ show (Z.getOffset z) ++ ", found '" ++ show (Z.maybeCurrent z) ++ "'"

word :: Parser String
word = Parser.takeWhile (not . isSpace)

token :: String -> Parser String
token s = do
  w <- word
  when (w /= s) (fail ("Expected token \"" ++ s ++ "\", found \"" ++ w ++ "\""))
  return w

takeWhile :: (Char -> Bool) -> Parser String
takeWhile f = Parser go
  where
    go z
        | null (Z.right z) = Right ([], z)
        | f c              = let (Right (s, z')) = go (Z.zipRight z) in Right (c : s, z')
        | otherwise        = Right ([], z)
      where
        c = Z.current z

whitespace :: Parser String
whitespace = Parser.takeWhile isSpace

ignoreWs :: Parser a -> Parser a
ignoreWs p = whitespace *> p <* whitespace

runParser :: Parser a -> String -> Either ErrorMsg (a, ZString)
runParser (Parser f) s = f (Z.fromList s)
-- }}}

-- elpenor parser {{{
identifier :: Parser AST
identifier = do
    w <- Parser.takeWhile isAlpha
    when (null w) (fail "Expected non-empty identifier")
    return $ Identifier w

number :: Parser AST
number = do
    n <- Parser.takeWhile isDigit
    when (null n) (fail "Expected non-empty number")
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
    _ <- char '('
    FunctionCall fname <$> parseArgs
  where
    parseArgs = ignoreWs expression >>= \e -> ([e] <$ char ')') <|> (char ',' >> ((e : ) <$> parseArgs))

expression :: Parser AST
expression = asum [functionCall, identifier, number, string]

assignment :: Parser AST
assignment = do
  _ <- ignoreWs (token "let")
  Identifier lhs <- ignoreWs identifier
  _ <- ignoreWs (char '=')
  rhs <- ignoreWs expression
  return $ Assignment lhs rhs

statement :: Parser AST
statement = asum [assignment, functionCall] <* char ';'

program :: Parser AST
program = Program <$> many statement
-- }}}

parse :: String -> AST
parse s = case runParser program s of
    Left e          -> error e
    Right (ast, z)  -> if null (Z.right z)
                         then ast
                         else error (show $ runParser statement s)

-- vim: foldmethod=marker
