{ pkgs, pname }:

pkgs.haskellPackages.callCabal2nix pname ./.. { }
