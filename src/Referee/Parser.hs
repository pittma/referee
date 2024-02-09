{-# LANGUAGE OverloadedStrings #-}
module Referee.Parser where

import Prelude hiding (fail)

import Data.Text
import Text.Parselet hiding (parser)
import Text.Read

data BuiltinOp
  = Add
  | Sub deriving (Show)


data Expr
  = App Text [Expr]
  | Var Text
  | SVal Text
  | IVal Integer
  | Builtin BuiltinOp [Expr]
  | Def Text [Text] Expr
  deriving (Show)

data Spec
  = SVar Text
  | And Spec Spec
  | Or Spec Spec
  | Not Spec
  deriving (Show, Eq)

parseExpr :: Text -> Maybe [Expr]
parseExpr t =
  fst
    <$> runParser
          t
          (do
             defs <- repeatUntil (whitespace >> defParser)
             whitespace
             eof
             pure defs)

defParser :: Parser Expr
defParser = do
  one '(' >> phrase_ "def" >> whitespace
  n <- word
  whitespace >> one '[' >> whitespace
  args <- repeatUntil var <|> (nop >> pure [])
  whitespace >> one ']' >> whitespace >> one '(' >> whitespace
  body <- parseBody
  whitespace >> one ')' >> one ')'
  pure (Def n args body)
  where
    var = whitespace >> word

parseBody :: Parser Expr
parseBody =
  (do
     one '('
     n <- word
     whitespace
     body <-
       repeatUntil
         (do
            b <- parseBody
            whitespace
            pure b)
     one ')'
     pure
       (case n of
          "add" -> Builtin Add body
          "sub" -> Builtin Sub body
          _ -> App n body))
    <|> (do
           one '\"'
           s <- repeatUntil alphanumeric
           one '\"' >> pure (SVal $ pack s))
    <|> (do
           v <- word
           case readMaybe (unpack v) :: Maybe Integer of
             Just i -> pure (IVal i)
             Nothing -> fail)
    <|> (Var <$> word)


parseSpec :: Text -> Maybe Spec
parseSpec t = fst <$> runParser t specParser

specParser :: Parser Spec
specParser =
  (do
     one '!'
     one '('
     s <- specParser
     one ')'
     con Not s <|> (eof >> pure (Not s)))
    <|> (do
           one '('
           s <- specParser
           one ')'
           con id s <|> (eof >> pure s))
    <|> (do
           v <- var
           whitespace
           c <- parseConnective
           whitespace
           c v <$> specParser)
    <|> var
  where
    con f s = do
      whitespace
      c <- parseConnective
      whitespace
      fmap (c (f s)) specParser
    var = (one '!' >> Not . SVar <$> word) <|> (SVar <$> word)

parseConnective :: Parser (Spec -> Spec -> Spec)
parseConnective = fmap f (phrase "||" <|> phrase "&&")
  where
    -- the parser above only eats if we get `||` or `&&`, so the
    -- partiality below is "okay".
    f t
      | t == "&&" = And
      | t == "||" = Or
