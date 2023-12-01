{-# LANGUAGE OverloadedStrings #-}
module Referee where

import Prelude hiding (repeat)
import Data.Text
import Text.Parselet

newtype Arg =
  Arg Text
  deriving (Show)
  
newtype Statement =
  Statement Text
  deriving (Show)

data Spec
  = Var Text
  | And Spec Spec
  | Or Spec Spec
  | Not Spec
  deriving (Show)

spec :: Parser Spec
spec =
  (do
     one '('
     s <- spec
     one ')'
     (do
        whitespace
        c <- parseConnective
        whitespace
        fmap (c s) spec)
       <|> pure s)
    <|> (one '!' >> Not <$> spec)
    <|> parseSpec

parseConnective :: Parser (Spec -> Spec -> Spec)
parseConnective = fmap f (phrase "||" <|> phrase "&&")
  where
    -- the parser above only eats if we get `||` or `&&`, so the
    -- partiality below is "okay".
    f t
      | t == "&&" = And
      | t == "||" = Or

parseSpec :: Parser Spec
parseSpec = assocr var cv
  where
    var = do
      whitespace
      v <-
        (do
           one '!'
           Not . Var <$> word)
          <|> (Var <$> word)
      whitespace
      pure v
    cv = parseConnective
