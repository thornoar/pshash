{ pkgs ? import <nixpkgs> { }, ... }:
import ./build/dynamic.nix {
  inherit pkgs;
  pname = "pshash";
}

# { pkgs ? import <nixpkgs> {} }:
# pkgs.stdenv.mkDerivation {
#     pname = "pshash";
#     version = "0.1.9.1";
#     src = ./app;
#     buildInputs = [
#         pkgs.ghc
#     ];
#     buildPhase = ''
#         ghc Main.hs -no-keep-hi-files -no-keep-o-files -o pshash
#     '';
#     installPhase = ''
#         mkdir -p $out/bin
#         mv pshash $out/bin/
#     '';
# }
