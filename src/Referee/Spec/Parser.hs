{-# LANGUAGE OverloadedStrings #-}
module Referee.Spec.Parser (Spec(..), parse) where

import Data.Text
import Text.Parselet hiding (parser)

data Spec
  = Var Text
  | And Spec Spec
  | Or Spec Spec
  | Not Spec
  deriving (Show, Eq)

parse :: Text -> Maybe Spec
parse t = fst <$> runParser t parser

parser :: Parser Spec
parser =
  (do
     one '!'
     one '('
     s <- parser
     one ')'
     con Not s <|> pure (Not s))
    <|> (do
           one '('
           s <- parser
           one ')'
           con id s <|> pure s)
    <|> (do
           v <- var
           whitespace
           c <- parseConnective
           whitespace
           c v <$> parser)
    <|> var
  where
    con f s = do
      whitespace
      c <- parseConnective
      whitespace
      fmap (c (f s)) parser
    var = (one '!' >> Not . Var <$> word) <|> (Var <$> word)

parseConnective :: Parser (Spec -> Spec -> Spec)
parseConnective = fmap f (phrase "||" <|> phrase "&&")
  where
    -- the parser above only eats if we get `||` or `&&`, so the
    -- partiality below is "okay".
    f t
      | t == "&&" = And
      | t == "||" = Or
