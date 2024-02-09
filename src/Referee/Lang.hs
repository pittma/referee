{-# LANGUAGE OverloadedStrings #-}
module Referee.Lang where

import qualified Data.Map as M
import Data.Text (Text)

import Referee.Parser
import Referee.Types

type Env = M.Map Text Expr
  
evalBuiltin :: BuiltinOp -> [Expr] -> Expr
evalBuiltin Add [IVal l, IVal r] = IVal (l + r)
evalBuiltin Sub [IVal l, IVal r] = IVal (l - r)

eval :: Env -> Expr -> Maybe Expr
eval env (Builtin op exprs) = do
  e <- mapM (eval env) exprs
  pure (evalBuiltin op e)
eval env (App n args) = do
  func <- M.lookup n env
  case func of
    (Def _ params body) -> eval (foldr f env (zip params args)) body
    _ -> Nothing
  where
    f (p, a) = M.insert p a
eval env (Var n) = M.lookup n env
eval _ x = Just x

run :: Text -> Maybe Expr
run t = do
  ast <- parseExpr t
  env <- Just (foldr f M.empty ast)
  case M.lookup "main" env of
    Just (Def _ a e) -> eval env e
    _ -> Nothing
  where
    f d@(Def n _ _) m = M.insert n d m
    f _ m = m
