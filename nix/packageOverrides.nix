{
  cmake,
  emacs,
  enchant2,
  gcc,
  libvterm-neovim,
  pkg-config,
  python3,
  pywal,
  substituteAll,
}: _final: prev: {
  bbdb = prev.bbdb.overrideAttrs (old: {
    preBuild = ''
      substituteInPlace bbdb-site.el.in \
          --replace "@pkgdatadir@" $out/share/emacs/site-lisp/tex/ \
          --replace "@PACKAGE_VERSION@" ${old.version};

      mv bbdb-site.el{.in,}
    '';
  });

  haskell-mode = prev.haskell-mode.overrideAttrs (_: {
    preBuild = ''
      substituteInPlace haskell-mode.el --replace "(require 'flymake)" "(require 'flymake)${"\n"}(require 'flymake-proc)"
    '';
  });

  jinx = prev.jinx.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkg-config];
    buildInputs = (old.buildInputs or []) ++ [enchant2];

    postBuild = ''
      NIX_CFLAGS_COMPILE="$($PKG_CONFIG --cflags enchant-2) $NIX_CFLAGS_COMPILE"
      $CC -shared -o jinx-mod.so jinx-mod.c -lenchant-2
    '';

    postInstall =
      (old.postInstall or "")
      + ''
        outd="$out/share/emacs/site-lisp"
        install -m444 -t $outd jinx-mod.so
        rm $outd/jinx-mod.c $outd/emacs-module.h
      '';
  });

  magit = prev.magit.overrideAttrs (old: {
    preBuild = ''
      substituteInPlace Makefile --replace "include ../default.mk" ""
      make PKG=magit VERSION="${old.version}" magit-version.el
      rm Makefile
    '';
  });

  theme-magic = prev.theme-magic.overrideAttrs (_: {
    patches = [
      (substituteAll {
        src = ./patches/theme-magic.patch;
        python = "${python3}/bin/python";
        wal = "${pywal}/bin/wal";
      })
    ];
  });

  vterm = prev.vterm.overrideAttrs (old: {
    nativeBuildInputs = [cmake gcc];
    buildInputs = old.buildInputs ++ [libvterm-neovim];
    cmakeFlags = ["-DEMACS_SOURCE=${emacs.src}"];
    preBuild = ''
      mkdir -p build
      cd build
      cmake ..
      make
      install -m444 -t . ../*.so
      install -m600 -t . ../*.el
      cp -r -t . ../etc
      rm -rf {CMake*,build,*.c,*.h,Makefile,*.cmake}
    '';
  });
}
