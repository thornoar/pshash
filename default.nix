{ pkgs ? import <nixpkgs> { }, ... }:
import ./build/dynamic.nix {
  inherit pkgs;
  pname = "pshash";
}
