module Filamento.Classes.Move where

import Filamento.Classes
import Filamento.Types.Quantities.Delta
import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Linear (V2 (V2))
import Relude

class Move2D a where
  moveLeft :: Length -> a -> a
  moveRight :: Length -> a -> a
  moveFront :: Length -> a -> a
  moveBack :: Length -> a -> a

instance Move2D (V2 Position) where
  moveLeft l (V2 x y) = V2 (sub x l) y
  moveRight l (V2 x y) = V2 (add x l) y
  moveFront l (V2 x y) = V2 x (add y l)
  moveBack l (V2 x y) = V2 x (sub y l)

instance Move2D (V2 Delta) where
  moveLeft l (V2 x y) = V2 (sub x l) y
  moveRight l (V2 x y) = V2 (add x l) y
  moveFront l (V2 x y) = V2 x (add y l)
  moveBack l (V2 x y) = V2 x (sub y l)