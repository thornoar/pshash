{
    description = "Password generator using hashing functions";

    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    };

    outputs = { self, nixpkgs }:
    let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
    in
    {
        packages.x86_64-linux.default = pkgs.stdenv.mkDerivation {
            pname = "pshash";
            version = "0.1.0";
            src = ./.;
            buildInputs = [ pkgs.ghc ];
            buildPhase = ''
                ghc pshash.hs -o pshash -no-keep-hi-files -no-keep-o-files
            '';
            installPhase = ''
                mkdir -p $out/bin
                cp pshash $out/bin
            '';
        };
    };
}
