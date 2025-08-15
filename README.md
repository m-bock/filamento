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
