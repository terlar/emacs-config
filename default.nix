{ version ? "dev", lib, stdenv, trivialBuild, emacs-all-the-icons-fonts, ripgrep
}:

let
  src = let filter = name: type: type != "symlink";
  in lib.sourceByRegex ./. [
    "early-init.el"
    "init.org"
    "lisp"
    "lisp/.*.el$"
    "snippets"
    "snippets/.*"
    "templates"
    "templates/.*"
  ];

  init = trivialBuild {
    pname = "config-init";
    inherit version src;

    preBuild = ''
      # Tangle org files
      emacs --batch -Q \
        -l org \
        *.org \
        -f org-babel-tangle

      # Fake config directory in order to have files on load-path
      mkdir -p .xdg-config
      ln -s $PWD .xdg-config/emacs
      export XDG_CONFIG_HOME="$PWD/.xdg-config"

      emacs --batch -Q \
        -l package \
        --eval '(setq package-quickstart t)' \
        -f package-quickstart-refresh
    '';
  };

  lisp = trivialBuild {
    pname = "config-lisp";
    inherit version;
    src = "${src}/lisp";
  };
in stdenv.mkDerivation {
  pname = "emacs-config";
  inherit src version;
  dontUnpack = true;

  buildInputs = [ emacs-all-the-icons-fonts ripgrep ];

  installPhase = ''
    install -D -t $out ${init}/share/emacs/site-lisp/*
    install -D -t $out/lisp ${lisp}/share/emacs/site-lisp/*
    cp -R $src/{snippets,templates} $out/.
  '';
}
