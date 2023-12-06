module Referee.Spec (
  module Referee.Spec.Parser,
  dnf
) where

import Data.Text
import Referee.Spec.Dnf (Dnf, dnf')
import Referee.Spec.Parser (parse)

dnf :: Text -> Maybe Dnf
dnf t = dnf' <$> parse t
