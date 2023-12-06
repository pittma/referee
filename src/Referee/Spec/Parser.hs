{-# LANGUAGE OverloadedStrings #-}
module Referee.Spec.Parser (Spec(..), parse) where

import Data.Text
import Text.Parselet

data Spec
  = Var Text
  | And Spec Spec
  | Or Spec Spec
  | Not Spec
  deriving (Show, Eq)

parse :: Parser Spec
parse =
  (do
     one '!'
     one '('
     s <- parse
     one ')'
     con Not s <|> pure (Not s))
    <|> (do
           one '('
           s <- parse
           one ')'
           con id s <|> pure s)
    <|> (do
           v <- var
           whitespace
           c <- parseConnective
           whitespace
           c v <$> parse)
    <|> var
  where
    con f s = do
      whitespace
      c <- parseConnective
      whitespace
      fmap (c (f s)) parse
    var = (one '!' >> Not . Var <$> word) <|> (Var <$> word)

parseConnective :: Parser (Spec -> Spec -> Spec)
parseConnective = fmap f (phrase "||" <|> phrase "&&")
  where
    -- the parser above only eats if we get `||` or `&&`, so the
    -- partiality below is "okay".
    f t
      | t == "&&" = And
      | t == "||" = Or
