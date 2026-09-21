{ pkgs, pname }:

let
  ncursesStatic = pkgs.ncurses.override { enableStatic = true; };
  haskellPackages = let
    fixGHC = pkg:
      pkg.override {
        enableRelocatedStaticLibs = true;
        enableShared = false;
        enableDwarf = false;
      };
  in pkgs.haskell.packages.ghc981.override (oldHP: {
    ghc = fixGHC oldHP.ghc;
    buildHaskellPackages =
      oldHP.buildHaskellPackages.override { ghc = fixGHC oldHP.ghc; };
  });
  pkg = haskellPackages.callCabal2nix pname ./.. { };
in pkgs.haskell.lib.overrideCabal pkg (old: {
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  isExecutable = true;
  # buildInputs = (old.buildInputs or []) ++ [ ncursesStatic ];
  configureFlags = [
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
    "--extra-lib-dirs=${
      pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })
    }/lib"
    "--extra-lib-dirs=${ncursesStatic}/lib"
  ];
})
