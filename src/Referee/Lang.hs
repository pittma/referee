{-# LANGUAGE OverloadedStrings #-}
module Referee.Lang where

import Prelude hiding (fail, repeat)

import Data.Text (Text, pack, unpack)
import qualified Data.Map as M
import Text.Read
  
import Text.Parselet hiding (parser)

data Expr
  = App Text [Expr]
  | Var Text
  | SVal Text
  | IVal Integer
  | Builtin BuiltinOp [Expr]
  | Def Text [Text] Expr
  deriving (Show)

data BuiltinOp
  = Add
  | Sub deriving (Show)

evalBuiltin :: BuiltinOp -> [Expr] -> Expr
evalBuiltin Add [IVal l, IVal r] = IVal (l + r)
evalBuiltin Sub [IVal l, IVal r] = IVal (l - r)

parse :: Text -> Maybe [Expr]
parse t =
  fst
    <$> runParser
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

type Env = M.Map Text Expr

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
  ast <- parse t
  env <- Just (foldr f M.empty ast)
  case M.lookup "main" env of
    Just (Def _ a e) -> eval env e
    _ -> Nothing
  where
    f d@(Def n _ _) m = M.insert n d m
    f _ m = m
