module Filamento.Types.Contexts where

import Filamento.Types.Quantities.Length
import Filamento.Types.Quantities.Position
import Linear (V2)
import Relude

-------------------------------------------------------------------------------

newtype Center = Center (V2 Position)

newtype Size = Size (V2 Length)

newtype FrontLeft = FrontLeft (V2 Position)

newtype FrontRight = FrontRight (V2 Position)

newtype BackRight = BackRight (V2 Position)

newtype BackLeft = BackLeft (V2 Position)

-- newtype Radius = Radius Length

-- newtype Diameter = Diameter Length
