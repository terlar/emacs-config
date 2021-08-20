{ stdenv
, fetchFromGitHub
, fetchFromGitLab
, fetchpatch
, substituteAll
, texinfo
, perl
, python3
, pywal
, which
, gcc
, pkg-config
, glib-networking
, gtk3
, webkitgtk
}:

epkgs:
let
  inherit (epkgs) trivialBuild;
in
epkgs // {
  theme-magic = epkgs.melpaPackages.theme-magic.overrideAttrs (attrs: {
    patches = [
      (substituteAll {
        src = ./patches/theme-magic.patch;
        python = "${python3}/bin/python";
        wal = "${pywal}/bin/wal";
      })
    ];
  });

  # Compatibility.
  nix-mode = epkgs.melpaPackages.nix-mode.overrideAttrs (attrs: {
    version = "20210111.808";
    src = fetchFromGitHub {
      owner = "terlar";
      repo = "nix-mode";
      rev = "57a0746e87e0c23f280d28c8d3fadedcc94cf0e2";
      sha256 = "1m2m1sdcrfcwj1swyanfvsh3cmk8iy6rff1xl0g4a3am06miywba";
      # date = 2021-01-11T08:08:08+01:00;
    };
  });

  # Forks.
  flymake-diagnostic-at-point =
    epkgs.melpaPackages.flymake-diagnostic-at-point.overrideAttrs (attrs: {
      version = "20190810.2232";
      src = fetchFromGitHub {
        owner = "terlar";
        repo = "flymake-diagnostic-at-point";
        rev = "8a4f5c1160cbb6c2464db9f5c104812b0c0c6d4f";
        sha256 = "17hkqspg2w1yjlcz3g6kxxrcz13202a1x2ha6rdp4f1bgam5lhzq";
        # date = 2019-08-10T22:32:04+02:00;
      };
    });

  relative-buffers = trivialBuild {
    pname = "relative-buffers";
    version = "20200908.1228";
    src = fetchFromGitHub {
      owner = "terlar";
      repo = "relative-buffers";
      rev = "32b306b640faed00ef95f06f9f802feb3240ac1b";
      sha256 = "0wzxnbbzzjkzrnfdbdn7k172ad6mnhq5y3swcbilnk1w1a1lzyhn";
      # date = 2020-09-08T12:28:37+02:00;
    };
    packageRequires = with epkgs; [ dash f s ];
  };

  ws-butler = epkgs.melpaPackages.ws-butler.overrideAttrs (attrs: {
    version = "20200403.107";
    src = fetchFromGitHub {
      owner = "hlissner";
      repo = "ws-butler";
      rev = "2bb49d3ee7d2cba133bc7e9cdac416cd1c5e4fe0";
      sha256 = "1ifrcxlb6hinjv4bn54c8fars4avcm5ijaj44h606mqymj37dvn1";
      # date = 2020-04-03T01:07:46-04:00;
    };
  });

  # New.
  all-the-icons-completion = trivialBuild rec {
    pname = "all-the-icons-completion";
    version = "20210729.0041";
    src = fetchFromGitHub {
      owner = "iyefrat";
      repo = "all-the-icons-completion";
      rev = "d1d4b2f0dfbfa94d33fe50e8089c06601adfe674";
      sha256 = "05nwi58l4y6l60rmsddf73s4awppgsyqarhiz14r21dcqq0kfa4g";
      # date = 2021-07-29T00:41:27+03:00;
    };
    packageRequires = with epkgs; [ all-the-icons ];
  };

  apheleia = trivialBuild rec {
    pname = "apheleia";
    version = "20210722.1916";
    src = fetchFromGitHub {
      owner = "raxod502";
      repo = "apheleia";
      rev = "52e0a140a8414699e2ed58000300f526995237f5";
      sha256 = "1wsbj6rmzx490p07qyig5djznpizhixqf4czyc12wimm47knrn46";
      # date = 2021-07-22T19:16:42-07:00;
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
    packageRequires = with epkgs; [ eglot ];
  };

  ejira = trivialBuild {
    pname = "ejira";
    version = "20210305.1818";
    src = fetchFromGitHub {
      owner = "nyyManni";
      repo = "ejira";
      rev = "a1d46d6959c42b14ccb0c3714209fac1b38dd3d4";
      sha256 = "172ryx07k30zs87768qrjrdsfkc2zvmrmnpnzxd2hlgp3i5875pk";
      # date = 2021-03-05T18:18:59+02:00;
    };
    packageRequires = with epkgs; [
      dash-functional
      f
      helm
      jiralib2
      language-detection
      org
      ox-jira
      s
    ];
  };

  explain-pause-mode = trivialBuild {
    pname = "explain-pause-mode";
    version = "20200727.227";
    src = fetchFromGitHub {
      owner = "lastquestion";
      repo = "explain-pause-mode";
      rev = "2356c8c3639cbeeb9751744dbe737267849b4b51";
      sha256 = "0frnfwqal9mrnrz6q4v7vcai26ahaw81894arff1yjw372pfgv7v";
      # date = 2020-07-27T02:27:40-07:00;
    };
  };

  ghelp = trivialBuild {
    pname = "ghelp";
    version = "20210727.1618";
    src = fetchFromGitHub {
      owner = "casouri";
      repo = "ghelp";
      rev = "e6f0674869b7382a688eac6d8fa6da00fb134a20";
      sha256 = "19aahddvpsczhb9qlgqzlphx9icknaj35xp0qkz612zsvzjk5bxr";
      # date = 2021-07-27T16:18:26-04:00;
    };
    packageRequires = with epkgs; [ eglot geiser helpful sly ];
  };

  ligature = trivialBuild {
    pname = "ligature";
    version = "20210508.1409";
    src = fetchFromGitHub {
      owner = "mickeynp";
      repo = "ligature.el";
      rev = "3923baf1fb9bf509cc95b4b14d7d0e2f7c88e53c";
      sha256 = "1zayga9072i4n6a90747d8h3w21zx042gnkcyimbb7jxxjyyiw7b";
      # date = 2021-05-08T08:14:09+01:00;
    };
  };

  puni = trivialBuild rec {
    pname = "puni";
    version = "20210821.10";
    src = fetchFromGitHub {
      owner = "AmaiKinono";
      repo = "puni";
      rev = "29ae364e9028460accd32b54d80d33a6b411602b";
      sha256 = "0cng3xp3x30y80c0mmnjjsvrdhp66gkgqqkk5c38a3ybi9m3xj9c";
      # date = 2021-08-21T00:10:26+08:00;
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
    packageRequires = with epkgs; [ quick-peek ];
  };

  vundo = trivialBuild {
    pname = "vundo";
    version = "20210707.2224";
    src = fetchFromGitHub {
      owner = "casouri";
      repo = "vundo";
      rev = "e136164f779e9a91ef722778acc704048ba0809a";
      sha256 = "1c2yqwjq7wizfcn94hk5wp5yn4xqaakxq27wm2r98qdh71ga7xhb";
      # date = 2021-07-07T22:24:35-04:00;
    };
  };

  webkit = trivialBuild {
    pname = "webkit";
    version = "20201206.1401";
    src = fetchFromGitHub {
      owner = "akirakyle";
      repo = "emacs-webkit";
      rev = "5f5f8c2b1f0c97a43533c1e16cb0dd93f75ea626";
      sha256 = "00dgzyyka9n9bwsdysv97cpwcczpr8m3k5s88z5lilidz22qc1rk";
      # date = 2020-12-06T14:01:38-07:00;
    };

    packageRequires = with epkgs; [ gtk3 webkitgtk ];

    postPatch = ''
      rm tests.el
      rm evil-collection-webkit.el
      rm webkit-ace.el
      rm webkit-dark.el
    '';

    buildInputs = [ gcc pkg-config glib-networking ];

    preBuild = ''
      make
    '';

    postInstall = ''
      install -m444 -t $out/share/emacs/site-lisp webkit-module.* *.{js,css}
    '';
  };
}
