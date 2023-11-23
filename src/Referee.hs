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

data Connective
  = And
  | Or
  deriving (Show)

data Spec
  = Var Text
  | Con Connective Spec Spec
  deriving (Show)

data Function = Function
  { function_name :: Text
  , function_spec :: Spec
  , function_args :: [Arg]
  , function_body :: Statement
  } deriving (Show)

data Ast = Ast
  { ast_fileName :: Text
  , ast_functions :: [Function]
  } deriving (Show)

spec :: Parser Spec
spec =
  (do
     one '('
     s <- spec
     one ')'
     whitespace
     c <- parseConnective
     whitespace
     fmap (Con c s) spec)
    <|> parseSpec

parseConnective :: Parser Connective
parseConnective = fmap f (phrase "||" <|> phrase "&&")
  where
    f t
      | t == "&&" = And
      | t == "||" = Or

parseSpec :: Parser Spec
parseSpec = assocr var cv
  where
    var = do 
      whitespace
      v <- word
      whitespace
      pure (Var v)
    cv = Con <$> parseConnective
