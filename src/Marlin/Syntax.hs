module Marlin.Syntax where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Relude
import Text.Printf (printf)

-- | A single G‑code or M‑code command.
data Cmd = Cmd
  { cmdId :: Char,
    cmdNum :: Int,
    cmdArgs :: Map Char ArgValue
  }
  deriving (Show, Eq, Generic)

-- | Values for command arguments.
data ArgValue
  = ArgInt Int
  | ArgDouble Double
  deriving (Show, Eq, Generic)

-- | A parsed line of G‑code, possibly with a comment.
data GCodeLine = GCodeLine
  { cmd :: Maybe Cmd,
    comment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- | Typeclass for things convertible to Text.
instance ToText ArgValue where
  toText (ArgInt i) = T.pack (show i)
  toText (ArgDouble d) = T.pack (printf "%.*f" (3 :: Int) d)

instance ToText Cmd where
  toText (Cmd cid num args) =
    let headText = T.singleton cid <> T.pack (show num)
        argsText =
          args
            & Map.toList
            & fmap (\(k, v) -> " " <> T.singleton k <> toText v)
            & T.concat
     in headText <> argsText

instance ToText GCodeLine where
  toText (GCodeLine Nothing Nothing) = ""
  toText (GCodeLine Nothing (Just c)) = "; " <> c
  toText (GCodeLine (Just c) mComment) =
    let base = toText c
        com = maybe "" (\t -> " ; " <> t) mComment
     in base <> com
