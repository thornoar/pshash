{
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable"; };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      packages.${system} = rec {
        pshash-static = import ./build/static.nix {
          pkgs = pkgs.pkgsMusl;
          pname = "pshash";
        };
        pshash-dynamic = import ./build/dynamic.nix {
          inherit pkgs;
          pname = "pshash";
        };
        pshash-gui = import ./build/pshash-gui.nix {
          inherit pkgs;
          pname = "pshash-gui";
          version = "1.0";
        };
        default = pshash-dynamic;
      };
      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/pshash";
      };
      devShells.${system} = {
        pshash = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            zlib
            (haskellPackages.ghcWithPackages (p: with p; [
              directory
              containers
            ]))
          ];
        };
        pshash-gui = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            bear
            coccinelle
            valgrind
            gcc
            wxGTK32
          ];
        };
      };
    };
}
