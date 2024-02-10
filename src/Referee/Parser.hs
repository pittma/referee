{-# LANGUAGE OverloadedStrings #-}
module Referee.Parser where

import Prelude hiding (fail, take)

import Control.Monad.Except (throwError)
import Data.Text
import Text.Parselet
import Text.Read

import Referee.Types

refMParse :: (Monad m) => Text -> Parser a -> RefM m a
refMParse t p =
  let res = runParser t p
   in case res of
        Left s -> throwError (Parse (unpack $ take 10 s))
        Right (s, _) -> pure s

parseExpr :: (Monad m) => Text -> RefM m [Expr]
parseExpr t =
  refMParse
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


parseSpec :: (Monad m) => Text -> RefM m Spec
parseSpec t = refMParse t specParser

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
