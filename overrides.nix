{ stdenv, fetchFromGitHub, writeText, texinfo, perl, which }:

self: super:

with super;

{
  # Fix code actions when using javascript-typescript-langserver.
  jsonrpc = let
    src = jsonrpc.src;
    patch = ./patches/jsonrpc.patch;
    patchedSrc = stdenv.mkDerivation {
      name = "emacs-${jsonrpc.pname}-${jsonrpc.version}-patched.el";
      inherit src;
      phases = [ "patchPhase" ];
      patchPhase = "patch $src -o $out < ${patch}";
    };
  in elpaBuild rec {
    inherit (jsonrpc) pname ename version meta;
    src = patchedSrc;
    packageRequires = [ emacs ];
  };

  # Personal forks.
  flymake-diagnostic-at-point = flymake-diagnostic-at-point.overrideAttrs
    (attrs: {
      version = "20190810.2232";
      src = fetchFromGitHub {
        owner = "terlar";
        repo = "flymake-diagnostic-at-point";
        rev = "8a4f5c1160cbb6c2464db9f5c104812b0c0c6d4f";
        sha256 = "17hkqspg2w1yjlcz3g6kxxrcz13202a1x2ha6rdp4f1bgam5lhzq";
        # date = 2019-08-10T22:32:04+02:00;
      };
    });

  org = stdenv.mkDerivation rec {
    pname = "emacs-org";
    version = "20200609.845";

    src = fetchFromGitHub {
      owner = "terlar";
      repo = "org-mode";
      rev = "5a26a6cf2783836bfa16e006607d06a911b97c56";
      sha256 = "15h1p8d3a4kw1lfkfcdnssvjzy02lbiwp32c1961ifqj16y22xcc";
      # date = 2020-06-09T08:45:13+02:00;
    };

    preBuild = ''
      makeFlagsArray=(
        prefix="$out/share"
        ORG_ADD_CONTRIB="org* ox*"
        GITVERSION="${version}"
        ORGVERSION="${version}"
      );
    '';

    preInstall = ''
      perl -i -pe "s%/usr/share%$out%;" local.mk
    '';

    buildInputs = [ emacs texinfo perl which ];

    meta = with stdenv.lib; {
      homepage = "https://elpa.gnu.org/packages/org.html";
      license = licenses.free;
    };
  };

  # Packages not in MELPA/GNU ELPA.
  apheleia = trivialBuild rec {
    pname = "apheleia";
    version = "20200526.1437";
    src = fetchFromGitHub {
      owner = "raxod502";
      repo = "apheleia";
      rev = "6bd69671796c3d232ffae42df6eecba4eb1f7cd2";
      sha256 = "1cq1rcg1hzc9szynci5rl7pp3fi7i5kq35jy60cfa9aymmxgvi76";
      # date = 2020-05-26T14:37:17-06:00;
    };
  };

  eglot-x = trivialBuild rec {
    pname = "eglot-x";
    version = "20200104.1435";
    src = fetchFromGitHub {
      owner = "nemethf";
      repo = "eglot-x";
      rev = "910848d8d6dde3712a2a2610c00569c46614b1fc";
      sha256 = "0sl6k5y3b855mbix310l9xzwqm4nb8ljjq4w7y6r1acpfwd7lkdc";
      # date = 2020-01-04T14:35:35+01:00;
    };
    packageRequires = [ eglot ];
  };

  ejira = trivialBuild {
    pname = "ejira";
    version = "20200206.2144";
    src = fetchFromGitHub {
      owner = "nyyManni";
      repo = "ejira";
      rev = "89f7c668caf0e46e929f2c9187b007eed6b5c229";
      sha256 = "0a97gx016byiy5fri8jf3x3sfd2h2iw79s6nxv9jigpkgxrkjg7b";
      # date = 2020-02-06T21:44:57+02:00;
    };
    packageRequires = [ org s f ox-jira dash jiralib2 language-detection ];
  };

  explain-pause-mode = trivialBuild {
    pname = "explain-pause-mode";
    version = "20200621.1043";
    src = fetchFromGitHub {
      owner = "lastquestion";
      repo = "explain-pause-mode";
      rev = "318dace6da1952675a890ef597a08cf18e2cbae1";
      sha256 = "1bdj8kwmy7xf02xdyh498wjzc2kp42rhr13hmdyaj1q7d2icvpxb";
      # date = 2020-06-21T10:43:16-07:00;
    };
  };

  ivy-ghq = trivialBuild {
    pname = "ivy-ghq";
    version = "20191231.1957";
    src = fetchFromGitHub {
      owner = "analyticd";
      repo = "ivy-ghq";
      rev = "78a4cd32a7d7556c7c987b0089ea354e41b6f901";
      sha256 = "1ddpdhg26nhqdd30k36c3mkciv5k2ca7vqmy3q855qnimir97zxz";
      # date = 2019-12-31T19:57:04-08:00;
    };
  };

  nix-flymake = trivialBuild {
    pname = "nix-flymake";
    version = "20200202.1654";
    src = fetchFromGitHub {
      owner = "tviti";
      repo = "nix-flymake";
      rev = "b5fb042732bef53b2869e673c9c4c5451045ac5c";
      sha256 = "1xdmlqgsy4qa16fhfp1as81jcwa4faszq0dvfidiihi8bs4d0xpg";
      # date = 2020-02-02T16:54:39-10:00;
    };
  };

  org-pretty-table = trivialBuild {
    pname = "org-pretty-table";
    version = "20200329.1831";
    src = fetchFromGitHub {
      owner = "fuco1";
      repo = "org-pretty-table";
      rev = "88380f865a79bba49e4f501b7fe73a7bfb03bd1a";
      sha256 = "0kynnja58r9cwfrxxyycg6z4hz9s5rzgn47i9yki7rlr80nyn2bf";
      # date = 2020-03-29T18:31:18+02:00;
    };
  };

  source-peek = trivialBuild {
    pname = "source-peek";
    version = "20170424.347";
    src = fetchFromGitHub {
      owner = "iqbalansari";
      repo = "emacs-source-peek";
      rev = "fa94ed1def1e44f3c3999d355599d1dd9bc44dab";
      sha256 = "14ai66c7j2k04a0vav92ybaikcc8cng5i5vy0iwpg7b2cws8a2zg";
      # date = 2017-04-24T03:47:10+05:30;
    };
    packageRequires = [ quick-peek ];
  };

  valign = trivialBuild {
    pname = "valign";
    version = "20200612.2148";
    src = fetchFromGitHub {
      owner = "casouri";
      repo = "valign";
      rev = "6578fe45b143886a963646711cf579240333b4eb";
      sha256 = "04vsp7lvmn7x2zk2n0g5bsmxxx8xa7wiav68f0fb7m7lhdanc8r9";
      # date = 2020-06-12T21:48:10-04:00;
    };
  };
}
