{
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05"; };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pname = "pshash";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      packages.${system} = rec {
        static = import ./build/static.nix {
          pkgs = pkgs.pkgsMusl;
          inherit pname;
        };
        dynamic = import ./build/dynamic.nix {
          inherit pkgs;
          inherit pname;
        };
        default = dynamic;
      };
      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/${pname}";
      };
      devShells.${system}.default = pkgs.mkShell {
        nativeBuildInputs = [
          (pkgs.haskellPackages.callCabal2nix "stan" (pkgs.fetchFromGitHub {
            owner = "kowainik";
            repo = "stan";
            rev = "dc0a3a5";
            hash = "sha256-5tBpDLzaNe2cT/z2lh4T6V/aAPcXWUQkuE7DSgZKY6A=";
          }) { })
          pkgs.cabal-install
          pkgs.ghc
        ];
      };
    };
}
