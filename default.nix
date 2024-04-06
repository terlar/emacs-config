{
  lib,
  stdenv,
  trivialBuild,
  emacs-all-the-icons-fonts,
  iosevka-bin,
  linkFarm,
  ripgrep,
  tree-sitter-grammars,
  writeText,
  xorg,
}:
let
  initTreesit = writeText "init-treesit.el" ''
    (add-to-list 'treesit-extra-load-path  "${
      lib.pipe tree-sitter-grammars [
        builtins.attrValues
        (builtins.filter lib.isDerivation)
        (map (drv: {
          name = builtins.concatStringsSep "" [
            "lib"
            (lib.removeSuffix "-grammar" (lib.getName drv))
            stdenv.targetPlatform.extensions.sharedLibrary
          ];
          path = "${drv}/parser";
        }))
        (linkFarm "treesit-grammars")
      ]
    }/")
  '';

  init = trivialBuild {
    pname = "config-init";
    version = "1";

    src = lib.sourceByRegex ./. [ "init.org" ];

    preBuild = ''
      emacs --batch --quick \
        --load org \
        *.org \
        --funcall org-babel-tangle

      cat ${initTreesit} >> init.el
    '';

    buildPhase = ''
      runHook preBuild

      export HOME="$(mktemp -d)"
      emacs -L . --batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el

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
    inherit init initTreesit;
  };

  installPhase = ''
    mkdir -p $out
    ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/site-lisp $out

    if [ -d "${init}/share/emacs/native-lisp" ]; then
      mkdir -p $out/eln-cache
      ${xorg.lndir}/bin/lndir -silent ${init}/share/emacs/native-lisp $out/eln-cache
    fi

    install -D -t $out $src/templates
  '';
}
