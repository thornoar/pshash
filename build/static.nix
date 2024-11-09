{ pkgs, pname }:

let
    haskellPackages =
        let fixGHC = pkg: pkg.override {
            enableRelocatedStaticLibs = true;
            enableShared = false;
            enableDwarf = false;
        };
        in pkgs.haskell.packages.ghc981.override (oldHP: {
            ghc = fixGHC oldHP.ghc;
            buildHaskellPackages = oldHP.buildHaskellPackages.override {
                ghc = fixGHC oldHP.ghc;
            };
        });
    pkg = haskellPackages.callCabal2nix pname ./.. {};
in
    pkgs.haskell.lib.overrideCabal pkg (old: {
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
            "--ghc-option=-optl=-static"  # <- Added
            "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"  # <- Added for libgmp
            "--extra-lib-dirs=${pkgs.zlib.static}/lib" # <- Added for zlib
            "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib" # <- Added for libffi
            # "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib" # <- Added for libcurses
        ];
    })
