module Referee.Spec where

import Data.Text
import Text.Parselet (runParser)

import Referee.Spec.Dnf (Dnf, dnf')
import Referee.Spec.Parser (parse)

dnf :: Text -> Maybe Dnf
dnf t = dnf' . fst <$> runParser t parse
