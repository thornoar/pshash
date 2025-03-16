{ pkgs, pname, version }:

pkgs.stdenv.mkDerivation {
    inherit pname;
    inherit version;
    src = ./app;
    buildInputs = with pkgs; [
        gcc
        wxGTK32
    ];
    buildPhase = ''
        g++ main.cpp -O5 -o pshash-gui $(wx-config --cxxflags --libs)
    '';
    installPhase = ''
        mkdir -p $out/bin
        mv pshash-gui $out/bin/
    '';
}
