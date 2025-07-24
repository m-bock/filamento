{-# LANGUAGE DeriveGeneric #-}

module Marlin.Syntax where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Relude

-- | A single G‑code or M‑code command.
data Cmd = Cmd
  { cmdId :: Char,
    cmdNum :: Int,
    cmdArgs :: Map Char ArgValue
  }
  deriving (Show, Eq, Generic)

data ArgValue
  = ArgInt Int
  | ArgDouble Double
  deriving (Show, Eq, Generic)

data GCodeLine
  = GCodeLine
  { cmd :: Maybe Cmd,
    comment :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance ToText GCodeLine where
  toText = undefined