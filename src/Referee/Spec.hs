module Referee.Spec (
  solve
) where

import qualified Data.Map as M
import Data.Text (Text)

import Referee.Dnf
import Referee.Parser (parseSpec)
import Referee.Types

solve :: Text -> Maybe Bool
solve t = do
  d <- dnf t
  pure (any g d)
  where
    g :: [Term] -> Bool
    g ts =
      case foldr f (State M.empty) ts of
        State _ -> True
        Contra -> False
    f :: Term -> Eval -> Eval
    f _ Contra = Contra
    f (T a) (State m) =
      case M.lookup a m of
        Just True -> State m
        Just False -> Contra
        Nothing -> State (M.insert a True m)
    f (N a) (State m) =
      case M.lookup a m of
        Just False -> State m
        Just True -> Contra
        Nothing -> State (M.insert a False m)
