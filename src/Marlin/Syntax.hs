module Marlin.Syntax
  ( RawGCodeCmd (..),
    ArgValue (..),
    RawGCodeLine (..),
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text as Text
import Relude
import Text.Printf (printf)

-- | A single G‑code or M‑code command.
data RawGCodeCmd = RawGCodeCmd
  { cmd :: Text,
    cmdArgs :: Map Char ArgValue
  }
  deriving (Show, Eq, Generic)

-- | Values for command arguments.
data ArgValue
  = ArgInt Int
  | ArgDouble Double
  | ArgFlag Bool
  deriving (Show, Eq, Generic)

-- | A parsed line of G‑code, possibly with a comment.
data RawGCodeLine = RawGCodeLine
  { cmd :: Maybe RawGCodeCmd,
    rawExtra :: Text,
    comment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToText ArgValue where
  toText (ArgInt i) = T.pack (show i)
  toText (ArgDouble d) = T.pack (printf "%.4f" d)
  toText (ArgFlag True) = "1"
  toText (ArgFlag False) = "0"

instance ToText RawGCodeCmd where
  toText (RawGCodeCmd cmd args) =
    let argsText =
          args
            & Map.toList
            & fmap (\(k, v) -> " " <> T.pack (printf "%9s" (Text.unpack (T.singleton k <> toText v))))
            & T.concat
     in Text.pack (printf "%-4s" (Text.unpack cmd)) <> argsText

instance ToText [RawGCodeLine] where
  toText linesList = unlines zipped
    where
      cmds = map (\x -> maybe "" toText x.cmd <> x.rawExtra) linesList
      maxLength = fromMaybe 0 $ maximumMay (map T.length cmds)
      comments = map (maybe "" (\c -> "   ; " <> c) . (\x -> x.comment)) linesList
      zipped = zipWith (\cmd comment -> cmd <> (T.replicate (maxLength - T.length cmd) " ") <> comment) cmds comments

maximumMay :: (Foldable t, Ord a) => t a -> Maybe a
maximumMay = foldl' go Nothing
  where
    go Nothing y = Just y
    go (Just x) y = Just (max x y)