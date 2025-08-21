module Filamento.Debug where

import Relude

(|>) :: (Show a) => a -> String -> a
value |> label =
  trace (label <> " = " <> show value) $ id value

infixl 1 |>