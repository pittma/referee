module Referee.Types where

import qualified Data.Map as M
import Data.Text (Text)

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

type Dnf = [[Term]]

data Term
  = T Text
  | N Text
  deriving (Show)


data Eval
  = Contra
  | State (M.Map Text Bool)
  deriving (Show)
