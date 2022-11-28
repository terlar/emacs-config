{
  version ? "dev",
  lib,
  stdenv,
  trivialBuild,
  emacs-all-the-icons-fonts,
  ripgrep,
}: let
  lisp = trivialBuild {
    pname = "config-lisp";
    inherit version;
    src = lib.sourceFilesBySuffices ./lisp [".el"];
  };

  init = trivialBuild {
    pname = "config-init";
    inherit version;

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

    src = lib.sourceByRegex ./. ["templates"];

    dontUnpack = true;

    buildInputs = [
      emacs-all-the-icons-fonts
      ripgrep
    ];

    passthru.components = {
      inherit lisp init;
    };

    installPhase = ''
      install -D -t $out/lisp ${lisp}/share/emacs/site-lisp/*
      install -D -t $out ${init}/share/emacs/site-lisp/*
      install -D -t $out $src/templates
    '';
  }
