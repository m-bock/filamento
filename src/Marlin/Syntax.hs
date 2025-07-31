module Marlin.Syntax where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Relude
import Text.Printf (printf)

-- | A single G‑code or M‑code command.
data RawGCodeCmd = RawGCodeCmd
  { cmdId :: Char,
    cmdNum :: Int,
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
  toText (ArgDouble d) = T.pack (printf "%.5f" d)
  toText (ArgFlag True) = "1"
  toText (ArgFlag False) = "0"

instance ToText RawGCodeCmd where
  toText (RawGCodeCmd cid num args) =
    let headText = T.singleton cid <> T.pack (show num)
        argsText =
          args
            & Map.toList
            & fmap (\(k, v) -> " " <> T.singleton k <> toText v)
            & T.concat
     in headText <> argsText

instance ToText [RawGCodeLine] where
  toText lines = unlines zipped
    where
      cmds = map (\x -> maybe "" toText x.cmd <> x.rawExtra) lines
      maxLength = fromMaybe 0 $ maximumMay (map T.length cmds)
      comments = map (maybe "" (\c -> " ; " <> c) . (\x -> x.comment)) lines
      zipped = zipWith (\cmd comment -> cmd <> (T.replicate (maxLength - T.length cmd) " ") <> comment) cmds comments

maximumMay :: (Foldable t, Ord a) => t a -> Maybe a
maximumMay = foldl' go Nothing
  where
    go Nothing y = Just y
    go (Just x) y = Just (max x y)