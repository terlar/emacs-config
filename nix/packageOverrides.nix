{
  stdenv,
  cmake,
  emacs,
  enchant2,
  gcc,
  libvterm-neovim,
  pkg-config,
  unzip,
}:
_final: prev: {
  jinx = prev.jinx.overrideAttrs (
    old:
    let
      moduleSuffix = stdenv.targetPlatform.extensions.sharedLibrary;
    in
    {
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkg-config ];
      buildInputs = (old.buildInputs or [ ]) ++ [ enchant2 ];

      preBuild = ''
        NIX_CFLAGS_COMPILE="$($PKG_CONFIG --cflags enchant-2) $NIX_CFLAGS_COMPILE"
        $CC -I. -O2 -fPIC -shared -o jinx-mod${moduleSuffix} jinx-mod.c -lenchant-2
        rm *.c *.h
      '';
    }
  );

  magit = prev.magit.overrideAttrs (old: {
    preBuild = ''
      substituteInPlace Makefile --replace "include ../default.mk" ""
      make PKG=magit VERSION="${old.version}" magit-version.el
      rm Makefile
    '';
  });

  nov = prev.nov.overrideAttrs (old: {
    propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [ unzip ];
  });

  vterm = prev.vterm.overrideAttrs (old: {
    nativeBuildInputs = [
      cmake
      gcc
    ];
    buildInputs = old.buildInputs ++ [ libvterm-neovim ];
    cmakeFlags = [ "-DEMACS_SOURCE=${emacs.src}" ];
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
