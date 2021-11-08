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

  indent-info = epkgs.melpaPackages.indent-info.overrideAttrs (attrs: {
    version = "20211108.2345";
    src = fetchFromGitHub {
      owner = "terlar";
      repo = "indent-info.el";
      rev = "b22ca7e401345d4abeb81038efd887d57a1ac4a6";
      sha256 = "0ixfy9k6a62rxc038fcrzy9pwf7b2qaxaxi0dh824hv87pbc8yys";
      # date = 2021-11-08T23:45:40+01:00;
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
    version = "20211018.52";
    src = fetchFromGitHub {
      owner = "hlissner";
      repo = "ws-butler";
      rev = "572a10c11b6cb88293de48acbb59a059d36f9ba5";
      sha256 = "14pis00zbcr0w0wxd25520apxavzs2v3b6p7jf7q1i3izdjb8k3k";
      # date = 2021-10-18T00:52:06+02:00;
    };
  });

  # New.
  apheleia = trivialBuild rec {
    pname = "apheleia";
    version = "20210808.607";
    src = fetchFromGitHub {
      owner = "raxod502";
      repo = "apheleia";
      rev = "8e022c67fea4248f831c678b31c19646cbcbbf6f";
      sha256 = "171wd33j7mifcr8hrd6n63wx6pikv9bj252z1fvqsmrv2h27zsry";
      # date = 2021-08-08T06:07:08-07:00;
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
    version = "20211007.923";
    src = fetchFromGitHub {
      owner = "casouri";
      repo = "ghelp";
      rev = "8ecf13657f6c4185cf5a2ddede184935d45bfe20";
      sha256 = "14kkzic3way92sz6h29ilklk5alz6rvid67zc9iyncyqk4qz055h";
      # date = 2021-10-07T09:23:43-07:00;
    };
    packageRequires = with epkgs; [ eglot geiser helpful sly ];
  };

  ligature = trivialBuild {
    pname = "ligature";
    version = "20210827.940";
    src = fetchFromGitHub {
      owner = "mickeynp";
      repo = "ligature.el";
      rev = "d3426509cc5436a12484d91e48abd7b62429b7ef";
      sha256 = "1a1sff38mwhpwz7dg9swp6q9zi1qwy9k48b4bcc66cqbyj8l78bd";
      # date = 2021-08-27T09:40:16+01:00;
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
    version = "20210830.1807";
    src = fetchFromGitHub {
      owner = "akirakyle";
      repo = "emacs-webkit";
      rev = "96a4850676b74ffa55b52ff8e9824f7537df6a47";
      sha256 = "0ifdngan6jhbz6p72igwvmz7lhmz7hl8ak5n7zjkvxmq05kxkc5a";
      # date = 2021-08-30T18:07:40-06:00;
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
