{-# LANGUAGE OverloadedStrings #-}
module Referee where

import Prelude hiding (and, not, or, repeat)
import Data.Text hiding (concatMap, map)
import Text.Parselet

data Spec
  = Var Text
  | And Spec Spec
  | Or Spec Spec
  | Not Spec
  deriving (Show)

spec :: Parser Spec
spec =
  (do
     one '!'
     one '('
     s <- spec
     one ')'
     con Not s <|> pure (Not s))
    <|> (do
           one '('
           s <- spec
           one ')'
           con id s <|> pure s)
    <|> (do
           v <- var
           whitespace
           c <- parseConnective
           whitespace
           c v <$> spec)
    <|> var
  where
    con f s = do
      whitespace
      c <- parseConnective
      whitespace
      fmap (c (f s)) spec
    var = (one '!' >> Not . Var <$> word) <|> (Var <$> word)

parseConnective :: Parser (Spec -> Spec -> Spec)
parseConnective = fmap f (phrase "||" <|> phrase "&&")
  where
    -- the parser above only eats if we get `||` or `&&`, so the
    -- partiality below is "okay".
    f t
      | t == "&&" = And
      | t == "||" = Or

type Dnf = [[Term]]

data Term
  = T Text
  | N Text
  deriving (Show)

dnf :: Spec -> Dnf
dnf (Or a b) = or (dnf a) (dnf b)
  where
    or xs ys = xs ++ ys
dnf (And a b) = and (dnf a) (dnf b)
  where
    and xs ys =
      let c = [(x, y) | x <- xs, y <- ys]
       in map (uncurry (++)) c
dnf (Var v) = [[T v]]
dnf (Not s) = not (dnf s)
  where
    not xs = or' $ concatMap (map negate) xs
    or' (x:xs) = [x] : or' xs
    or' [] = []
    negate (T v) = N v
    negate (N v) = T v

dnf' :: Text -> Maybe Dnf
dnf' t = dnf . fst <$> runParser t spec
