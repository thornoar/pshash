## Synopsis

`pshash` is a pseudo-hash algorithm implemented in Haskell, that serves as a password manager by accepting three keys (one public and two private) and returning a pseudo-hash that can be used as a password. The program does not store the passwords anywhere, instead it generates them on the fly every time, which ensures a significant degree of security. The algorithm was designed to withstand brute-forcing as well. For finer detail, please refer to the corresponding mathematical paper: *documentation/main.pdf*

## What do I mean by 'pseudo-hash'?

Strictly speaking, a hash algorithm represents a mathematical function that is injective (i.e. does not map different inputs to the same output) and at the same time impossible to invert. This non-invertibility is usually achieved through manipulation of particular bits of the input on the low level and in an imperative way. From a purely functional approach, this is hard to realize, especially on such a high-level language as Haskell. Hence, the initial algorithm used in `pshash` to combine public and private keys into a password is a one-to-one, yet perfectly invertible function. It is then artificially made non-injective by the second private key, to "erase the traces" of the first private key. Due to its structure, this function cannot be formally called a hash function.

## Where can you get the passwords?

First of all, you can use the algorithm online at https://thornoar.github.io/pshash/web-app/. Moreover, `pshash` adopts a wide range of local deployment options:
- You can build `pshash` with Nix flakes by invoking `nix build github:thornoar/pshash`, there are two outputs: `#dynamic` for dynamic linking, and `#static` for static linking (will take a lot longer to build).
- You can install `pshash` with `cabal-install` by cloning this repo and running `cabal update && cabal install`.
- You can simply compile the Haskell file `app/Main.hs` with `ghc`, given that it finds the necessary libraries.
- You can directly download the relevant executables from the `pshash-bin` repo: https://github.com/thornoar/pshash-bin

## Version details

You can contact me via email: `r.a.maksimovich@gmail.com`, or on Telegram: `@thornoar`.

The latest version is 0.1.2.1, available on NixOS, Debian and Windows. The Fedora, Android and Arch binaries are out-of-date.
