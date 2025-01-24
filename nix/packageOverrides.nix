{
  lib,
  stdenv,
  cmake,
  copilot-node-server,
  emacs,
  enchant2,
  gcc,
  libvterm-neovim,
  pkg-config,
  nodejs,
  unzip,
  fetchFromGitHub,
}:

let
  # The Emacs package isn't compatible with the latest
  # copilot-node-server so we have to set a specific revision
  # https://github.com/copilot-emacs/copilot.el/issues/344
  pinned-copilot-node-server = copilot-node-server.overrideAttrs (old: rec {
    version = "1.27.0";
    src = fetchFromGitHub {
      owner = "jfcherng";
      repo = "copilot-node-server";
      rev = version;
      hash = "sha256-Ds2agoO7LBXI2M1dwvifQyYJ3F9fm9eV2Kmm7WITgyo=";
    };
    meta = lib.recursiveUpdate old.meta {
      unfree = false;
      license.free = true;
    };
  });
in
_final: prev: {
  cape = prev.cape.overrideAttrs (_: {
    preBuild = ''
      substituteInPlace cape-char.el \
        --replace-fail "when-let" "when-let*"
    '';
  });

  copilot = prev.copilot.overrideAttrs (_: {
    preBuild = ''
      sed -iE '/defcustom copilot-install-dir.*/{N;s|.*|(defcustom copilot-install-dir "${pinned-copilot-node-server}"|;}' copilot.el

      substituteInPlace copilot.el \
        --replace-fail \
        '(executable-find "node")' \
        '"${nodejs}/bin/node"'
    '';
  });

  corfu = prev.corfu.overrideAttrs (_: {
    preBuild = ''
      substituteInPlace corfu-popupinfo.el \
        --replace-fail "if-let" "if-let*" \
        --replace-fail "when-let" "when-let*"
    '';
  });

  devdocs = prev.devdocs.overrideAttrs (_: {
    preBuild = ''
      substituteInPlace devdocs.el --replace-fail "(require 'mathjax)" "(require 'mathjax nil t)"
    '';
  });

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
