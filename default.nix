{
  lib,
  stdenv,
  trivialBuild,
  emacs-all-the-icons-fonts,
  emacsPackages,
  iosevka-bin,
  ripgrep,
  xorg,
}:
let
  tree-sitter = emacsPackages.treesit-grammars.with-all-grammars;

  init = trivialBuild {
    pname = "config-init";
    version = "1";

    src = lib.sourceByRegex ./. [ "init.org" ];

    preBuild = ''
      emacs --batch --quick \
        --load org \
        *.org \
        --funcall org-babel-tangle
    '';

    buildPhase = ''
      runHook preBuild

      export HOME="$(mktemp -d)"

      mkdir -p "$HOME/.emacs.d"
      ln -s ${tree-sitter}/lib "$HOME/.emacs.d/tree-sitter"

      emacs --batch --quick \
        --eval '(setq byte-compile-error-on-warn t)' \
        --funcall batch-byte-compile \
        *.el

      runHook postBuild
    '';

    # Temporary hack because the Emacs native load path is not respected.
    fixupPhase = ''
      if [ -d "$HOME/.emacs.d/eln-cache" ]; then
        mv $HOME/.emacs.d/eln-cache/* $out/share/emacs/native-lisp
      fi
    '';
  };
in
stdenv.mkDerivation {
  name = "emacs-config";

  src = lib.sourceByRegex ./. [ "templates" ];
  dontUnpack = true;

  buildInputs = [
    (iosevka-bin.override { variant = "Aile"; })
    (iosevka-bin.override { variant = "CurlySlab"; })
    (iosevka-bin.override { variant = "Etoile"; })
    emacs-all-the-icons-fonts
    ripgrep
  ];

  passthru.components = {
    inherit init;
  };

  installPhase = ''
    mkdir -p $out
    ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/site-lisp $out

    if [ -d "${init}/share/emacs/native-lisp" ]; then
      mkdir -p $out/eln-cache
      ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/native-lisp $out/eln-cache
    fi

    install -D -t $out $src/templates

    ln -s ${tree-sitter}/lib $out/tree-sitter
  '';
}
