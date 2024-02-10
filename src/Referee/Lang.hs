{-# LANGUAGE OverloadedStrings #-}
module Referee.Lang where

import Control.Monad.Except
import qualified Data.Map as M
import Data.Text (Text, unpack)

import Referee.Parser
import Referee.Types

type Env = M.Map Text Expr
  
evalBuiltin :: BuiltinOp -> [Expr] -> Expr
evalBuiltin Add [IVal l, IVal r] = IVal (l + r)
evalBuiltin Sub [IVal l, IVal r] = IVal (l - r)

envLookup :: (Monad m) => Env -> Text -> RefM m Expr
envLookup env k = 
  let v = M.lookup k env
   in case v of
    Just d -> pure d
    _ -> throwError (Undefined (unpack k))

eval :: (Monad m) => Env -> Expr -> RefM m Expr
eval env (Builtin op exprs) = do
  e <- mapM (eval env) exprs
  pure (evalBuiltin op e)
eval env (App n args) = do
  func <- envLookup env n
  case func of
    (Def _ params body) -> eval (foldr f env (zip params args)) body
    _ -> throwError (Undefined (unpack n))
  where
    f (p, a) = M.insert p a
eval env (Var n) = envLookup env n
eval _ x = pure x

run :: (Monad m) => Text -> RefM m Expr
run t = do
  ast <- parseExpr t
  let env = foldr f M.empty ast
  main <- envLookup env "main"
  case main of
    (Def _ a e) -> eval env e
    _ -> throwError Main
  where
    f d@(Def n _ _) m = M.insert n d m
    f _ m = m
