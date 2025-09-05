# filamento

A functional and typed G-code generator for 3D printers.

![filamento](assets/logo.png)

## Quick Start

### Installation

```bash
cabal install -n
```

### Using as a Dependency

To use Filamento in your own project, create a `cabal.project` file:

```cabal
packages: .

source-repository-package
  type: git
  location: git@github.com:m-bock/filamento.git
  tag: main
```

Then add `filamento` to `build-depends` in your regular `*.cabal` file:

```cabal
build-depends: base ^>=4.17.2.1, filamento
```

### Minimal Example

```haskell
module Readme where

import Filamento
import Relude

sketch :: GCode ()
sketch = initPrinter do
  comment "This is a sample sketch"

  printTestStripes

  moveToZ (fromMm 0.2)

  forM_ [1 .. 20] \i -> do
    comment $ "Layer " <> show i
    nextLayer

    let p1 = v2PosFromMm 50 60
        p2 = v2PosFromMm 75 25
        p3 = v2PosFromMm 80 80

    moveTo p1
    extrudeTo p2
    extrudeTo p3
    extrudeTo p1

  hookEmitGCode "final"

main :: IO ()
main = do
  hooks <- mkHookFileAppender "out/sample.gcode"

  gcodeLaunch
    sketch
    ( \env ->
        env
          { hookEmitGCode = hooks,
            lineWidth = fromMm 0.6,
            layerHeight = fromMm 0.3,
            firstLayerHeight = fromMm 0.3,
            hotendTemperature = fromCelsius 205,
            bedTemperature = fromCelsius 65
          }
    )
```

```gcode
G21
M83
M140       S65
M104      S205
G28
M190       S65
M109      S205
                                                         ; This is a sample sketch
G1     E0.0000     F9000 X145.5000  Y94.0000   Z0.2000
G1     E0.0000     F1020   X5.0000   Y5.0000   Z0.2000
G1     E5.0000      F780   X5.0000   Y5.0000   Z0.2000
G1    E15.7154      F780   X5.0000 Y215.0000   Z0.2000
G1    E-1.0000      F780   X5.0000 Y215.0000   Z0.2000
G1    E-1.0000      F780   X5.0000 Y215.0000   Z0.2000
G1     E0.0000     F1020   X5.0000 Y215.0000   Z0.6000
G1     E0.0000     F9000  X10.0000   Y5.0000   Z0.6000
G1     E0.0000     F9000  X10.0000   Y5.0000   Z0.2000
G1     E1.0000      F780  X10.0000   Y5.0000   Z0.2000
G1    E15.7154      F780  X10.0000 Y215.0000   Z0.2000
G1     E0.0000     F1020  X10.0000 Y215.0000   Z0.2000
                                                         ; Layer 1
G1     E0.0000     F1020  X10.0000 Y215.0000   Z0.3000
G1     E0.0000     F1020  X50.0000  Y60.0000   Z0.3000
G1     E3.2188      F780  X75.0000  Y25.0000   Z0.3000
G1     E4.1329      F780  X80.0000  Y80.0000   Z0.3000
G1     E2.6982      F780  X50.0000  Y60.0000   Z0.3000
                                                         ; Layer 2
G1     E0.0000     F1020  X50.0000  Y60.0000   Z0.6000
G1     E0.0000     F9000  X50.0000  Y60.0000   Z0.6000
G1     E3.2188     F2100  X75.0000  Y25.0000   Z0.6000
G1     E4.1329     F2100  X80.0000  Y80.0000   Z0.6000
G1     E2.6982     F2100  X50.0000  Y60.0000   Z0.6000
                                                         ; Layer 3
G1     E0.0000     F9000  X50.0000  Y60.0000   Z0.9000
G1     E0.0000     F9000  X50.0000  Y60.0000   Z0.9000
G1     E3.2188     F2100  X75.0000  Y25.0000   Z0.9000
G1     E4.1329     F2100  X80.0000  Y80.0000   Z0.9000
G1     E2.6982     F2100  X50.0000  Y60.0000   Z0.9000
                                                         ; Layer 4
G1     E0.0000     F9000  X50.0000  Y60.0000   Z1.2000
G1     E0.0000     F9000  X50.0000  Y60.0000   Z1.2000
G1     E3.2188     F2100  X75.0000  Y25.0000   Z1.2000
G1     E4.1329     F2100  X80.0000  Y80.0000   Z1.2000
G1     E2.6982     F2100  X50.0000  Y60.0000   Z1.2000
```
