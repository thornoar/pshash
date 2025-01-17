{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

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
    };
}
