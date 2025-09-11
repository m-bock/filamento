module GCodeViewer.TagName where

import Prelude

import Data.Argonaut.Core (Json)

class TagName a where
  tagName :: a -> { tag :: String, args :: Json }
