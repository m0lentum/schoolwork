{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:

let
  texlive_ = with pkgs; texlive.combine {
    inherit (texlive)
      scheme-basic
      latexmk
      # gradu3.cls dependencies
      biber biblatex-chicago biblatex
      xstring
      collection-fontsrecommended
      csquotes
      latexindent
      etoolbox
      ifthenx
      chngcntr
      geometry
      courier
      hyperref
      # ghostscript / octave publish dependencies
      listings
      mathtools
      titlesec
      # for fonts
      lato
      fontaxes metafont xkeyval xcolor xetex fontspec euenc unicode-math
      # for layout & formatting
      setspace ragged2e ms footmisc
      babel babel-finnish hyphen-finnish
      # other
      pgfplots
      fancyvrb
      algorithmicx
      beamer
      float
      ;
  };

  jupyter = pkgs.jupyter.override {
    definitions = {
      python3 =
        let
          env = (pkgs.python3.withPackages (ps: with ps; [
            numpy
            scipy
            matplotlib
            ipykernel
          ]));
        in
        {
          displayName = "Python 3";
          argv = [
            "${env.interpreter}"
            "-m"
            "ipykernel_launcher"
            "-f"
            "{connection_file}"
          ];
          language = "python";
          logo32 = "${env.sitePackages}/ipykernel/resources/logo-32x32.png";
          logo64 = "${env.sitePackages}/ipykernel/resources/logo-64x64.png";
        };
    };
  };

  # python with packages used on courses
  python = pkgs.python3.withPackages (ps: with ps; [
    simpy
    numpy
    jupytext
  ]);

  # helper to remove tex auxiliary files.
  # use with caution, make sure no wanted files have the same name
  cleantex = pkgs.writeScriptBin "cleantex" ''
    #!${pkgs.stdenv.shell}
    if [ -z $1 ]; then
      echo "give filename without extension please"
      exit 1
    fi
    ls $1.* | rg -v "\.tex" | xargs -I {} rm {}
  '';
in
pkgs.mkShell {
  buildInputs = [
    pkgs.niv

    jupyter
    python
    pkgs.octave
    # for publishing octave to pdf
    pkgs.ghostscript
    # for latex
    texlive_
    pkgs.pandoc
    pkgs.watchexec
    cleantex
    # for haskell
    pkgs.ghc
    pkgs.ormolu
  ];

  FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ pkgs.lato ]; };
}
