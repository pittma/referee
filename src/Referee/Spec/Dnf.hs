module Referee.Spec.Dnf where

import Prelude hiding (and, not, or)
import Data.Text hiding (map, concatMap)

import Referee.Spec.Parser

type Dnf = [[Term]]

data Term
  = T Text
  | N Text
  deriving (Show)

dnf' :: Spec -> Dnf
dnf' (Or a b) = or (dnf' a) (dnf' b)
  where
    or xs ys = xs ++ ys
dnf' (And a b) = and (dnf' a) (dnf' b)
  where
    and xs ys =
      let c = [(x, y) | x <- xs, y <- ys]
       in map (uncurry (++)) c
dnf' (Var v) = [[T v]]
dnf' (Not (Var v)) = [[N v]]
dnf' (Not s) = dnf' $ dist s
  where
    -- The best option for negated subexprs is to distribute the
    -- negation first, and then convert to dnf.
    dist :: Spec -> Spec
    dist (Not var@(Var _)) = var
    dist (Not s) = dist s
    dist (Or a b) = And (dist a)  (dist b)
    dist (And a b) = Or (dist a) (dist b)
    dist var@(Var _) = Not var
