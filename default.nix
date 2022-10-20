{
  version ? "dev",
  packageRequires,
  lib,
  stdenv,
  trivialBuild,
  emacs-all-the-icons-fonts,
  ripgrep,
}: let
  package-quickstart = trivialBuild {
    pname = "config-package-quickstart";
    inherit version packageRequires;

    dontUnpack = true;

    buildPhase = ''
      emacs --batch --quick \
        --load package \
        --eval '(setq package-quickstart-file "package-quickstart.el")' \
        --eval '(setq package-quickstart t)' \
        --funcall package-quickstart-refresh
    '';
  };

  lisp = trivialBuild {
    pname = "config-lisp";
    inherit version;
    src = lib.sourceFilesBySuffices ./lisp [".el"];
  };

  init = trivialBuild {
    pname = "config-init";
    inherit version packageRequires;

    src = lib.sourceByRegex ./. ["init.org" "lisp" "lisp/.*.el$"];

    buildPhase = ''
      emacs --batch --quick \
        --load org \
        *.org \
        --funcall org-babel-tangle

      mkdir -p .xdg-config
      ln -s $PWD .xdg-config/emacs
      export XDG_CONFIG_HOME="$PWD/.xdg-config"

      emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
    '';
  };
in
  stdenv.mkDerivation {
    pname = "emacs-config";
    inherit version;

    src = lib.sourceByRegex ./. [
      "snippets"
      "snippets/.*"
      "templates"
      "templates/.*"
    ];

    dontUnpack = true;

    buildInputs = [
      emacs-all-the-icons-fonts
      ripgrep
    ];

    passthru.components = {
      inherit package-quickstart lisp init;
    };

    installPhase = ''
      install -D -t $out ${package-quickstart}/share/emacs/site-lisp/*
      install -D -t $out/lisp ${lisp}/share/emacs/site-lisp/*
      install -D -t $out ${init}/share/emacs/site-lisp/*
      cp -R $src/{snippets,templates} $out/.
    '';
  }
