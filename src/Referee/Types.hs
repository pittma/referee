module Referee.Types where

import Control.Exception
import Control.Monad.Except
import qualified Data.Map as M
import Data.Text (Text)
import GHC.IO.Exception (ioe_description)

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

data Error
  = Parse String
  | Undefined String
  | IOError String
  | Main

instance Show Error where
  show (Parse s) = "Parse failure near: \"" ++ s ++ "\""
  show (Undefined s) = s ++ " is not defined as a function"
  show (IOError s) = "IO Error: " ++ s
  show Main = "main must be defined as a function"

type RefM m a = ExceptT Error m a

runRefM :: RefM m a -> m (Either Error a)
runRefM = runExceptT

refMIO :: (MonadIO m) => IO a -> RefM m a
refMIO io = do
  r <- liftIO $ try io
  case r of
    Left e -> throwError (IOError (ioe_description e))
    Right res -> pure res
