# Bus Map Haskell

## Setup

Following these steps you should be able to setup your development environment.

1. Install `ghcup`.
2. Using `ghcup` install `stack`.
3. Use `stack build` command inside project's directory. Note which GHC version was installed or used.
4. Find which haskell-language-server version provides for your GHC version [HLS Version Support page for GHC](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html). Otherwise, you can compile the version yourself (e.g. HLS 2.10.0.0, GHC 9.6.3):

```sh
ghcup compile hls -g 2.10.0.0 --ghc 9.6.3 --cabal-update
```
