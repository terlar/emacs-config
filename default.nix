{ version ? "dev", stdenv, trivialBuild, emacs-all-the-icons-fonts }:

let
  src = let filter = name: type: type != "symlink";
  in builtins.filterSource filter ./.;

  init = trivialBuild {
    pname = "emacs-config-init";
    inherit version src;

    preBuild = ''
      # Tangle org files
      emacs --batch -Q \
        *.org \
        -f org-babel-tangle

      # Fake config directory in order to have files on load-path
      mkdir -p .xdg-config
      ln -s $PWD .xdg-config/emacs
      export XDG_CONFIG_HOME="$PWD/.xdg-config"
    '';
  };

  lisp = trivialBuild {
    pname = "emacs-config-lisp";
    inherit version;
    src = "${src}/lisp";
  };
in stdenv.mkDerivation {
  pname = "emacs-config";
  inherit src version;
  dontUnpack = true;

  buildInputs = [ emacs-all-the-icons-fonts ];

  installPhase = ''
    install -D -t $out ${init}/share/emacs/site-lisp/*
    install -D -t $out/lisp ${lisp}/share/emacs/site-lisp/*
    cp -R $src/{snippets,templates} $out/.
  '';
}
