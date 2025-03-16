{ pkgs ? import <nixpkgs> { }, ... }:
import ./build/pshash-dynamic.nix {
  inherit pkgs;
  pname = "pshash";
}
