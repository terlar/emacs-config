{
  lib,
  stdenv,
  buildElispPackage,
  elispInputs,
  emacs-all-the-icons-fonts,
  treesit-grammars,
  xorg,
}:
let
  init = buildElispPackage {
    ename = "config-init";

    src = lib.sourceByRegex ./. [ "init.org" ];
    files = [ "init.org" ];
    lispFiles = [
      "early-init.el"
      "init.el"
    ];

    inherit elispInputs;
    nativeCompileAhead = true;
    wantExtraOutputs = false;
    errorOnWarn = true;
    doTangle = false;

    preBuild = ''
      export HOME="$NIX_BUILD_TOP/.home"
      mkdir -p "$HOME/.config/emacs"

      emacs --batch --quick \
        --load org \
        *.org \
        --funcall org-babel-tangle
      rm *.org

      ln -s ${treesit-grammars}/lib "$HOME/.config/emacs/tree-sitter"
    '';

    meta = { };
  };
in
stdenv.mkDerivation {
  name = "emacs-config";

  src = lib.sourceByRegex ./. [ "templates" ];
  dontUnpack = true;

  passthru = {
    components = {
      inherit init;
    };

    runtimeInputs = [
      emacs-all-the-icons-fonts
    ];
  };

  installPhase = ''
    mkdir -p $out
    ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/site-lisp $out

    if [ -d "${init}/share/emacs/native-lisp" ]; then
      mkdir -p $out/eln-cache
      ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/native-lisp $out/eln-cache
    fi

    install -D -t $out $src/templates

    ln -s ${treesit-grammars}/lib $out/tree-sitter
  '';
}
