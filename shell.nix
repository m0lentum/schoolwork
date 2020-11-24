let
  pkgs = import (fetchTarball {
    # nixos-20.03
    url = "https://github.com/NixOS/nixpkgs-channels/archive/ab3adfe1c769c22b6629e59ea0ef88ec8ee4563f.tar.gz";
    sha256 = "1m4wvrrcvif198ssqbdw897c8h84l0cy7q75lyfzdsz9khm1y2n1";
  }) {};

  texlive_ = with pkgs; texlive.combine {
    inherit (texlive)
      scheme-basic
      # gradu3.cls dependencies
      biber biblatex-chicago biblatex
      collection-fontsrecommended
      csquotes
      latexmk latexindent
      etoolbox
      ifthenx
      chngcntr
      geometry
      courier
      hyperref
      # for fonts
      lato
      fontaxes metafont xkeyval xcolor xetex fontspec euenc unicode-math
      # for layout & formatting
      setspace ragged2e ms footmisc
      babel babel-finnish
    ;
  };
in
pkgs.mkShell {
  buildInputs = [
    texlive_
    pkgs.pandoc
    pkgs.watchexec
  ];

  FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ pkgs.lato ]; };
}