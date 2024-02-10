module Referee.Spec (
  solve
) where

import qualified Data.Map as M
import Data.Text (Text)

import Referee.Parser (parseSpec)
import Referee.Types

dnf :: (Monad m) => Text -> RefM m Dnf
dnf t = dnf' <$> parseSpec t
  where
    dnf' :: Spec -> Dnf
    dnf' (Or a b) = or (dnf' a) (dnf' b)
    dnf' (And a b) = and (dnf' a) (dnf' b)
    dnf' (SVar v) = [[T v]]
    dnf' (Not (SVar v)) = [[N v]]
    dnf' (Not s) = dnf' $ dist s
    or xs ys = xs ++ ys
    and xs ys =
      let c = [(x, y) | x <- xs, y <- ys]
       in map (uncurry (++)) c
    dist :: Spec -> Spec
    dist (Not var@(SVar _)) = var
    dist (Not s) = dist s
    dist (Or a b) = And (dist a) (dist b)
    dist (And a b) = Or (dist a) (dist b)
    dist var@(SVar _) = Not var

solve :: (Monad m) => Text -> RefM m Bool
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
