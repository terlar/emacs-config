{
  stdenv,
  fetchFromGitHub,
  fetchFromGitLab,
  fetchpatch,
  substituteAll,
  texinfo,
  perl,
  python3,
  pywal,
  which,
  gcc,
  pkg-config,
  glib-networking,
  gtk3,
}: epkgs: let
  inherit (epkgs) trivialBuild;
in
  epkgs
  // {
    theme-magic = epkgs.melpaPackages.theme-magic.overrideAttrs (attrs: {
      patches = [
        (substituteAll {
          src = ./patches/theme-magic.patch;
          python = "${python3}/bin/python";
          wal = "${pywal}/bin/wal";
        })
      ];
    });

    # Temporary fixes for stale MELPA sync.
    haskell-mode = epkgs.melpaPackages.haskell-mode.overrideAttrs (attrs: {
      version = "20220917.1553";
      src = fetchFromGitHub {
        owner = "haskell";
        repo = "haskell-mode";
        rev = "90503413f4cdb0ed26871e39c4e6e2552b57f7db";
        sha256 = "19f9zqb6pfa71hpv4vk7ym8hcsy8hn0jmp3k2xk36vc38g1mr8ma";
        # date = 2022-09-17T15:53:59+02:00;
      };
    });

    # Forks.
    flymake-diagnostic-at-point = epkgs.melpaPackages.flymake-diagnostic-at-point.overrideAttrs (attrs: {
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
      packageRequires = with epkgs; [dash f s];
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
    corfu-doc = trivialBuild rec {
      pname = "corfu-doc";
      version = "20220108.1333";
      src = fetchFromGitHub {
        owner = "galeo";
        repo = "corfu-doc";
        rev = "488da677535b02e6e3b62b802928a6f9a61d33bf";
        sha256 = "sha256-5hMvAM2Sya/b2z2T8BvfOv18J2BDhpxAw0Vu2geApz0=";
      };
      packageRequires = with epkgs; [corfu];
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
      packageRequires = with epkgs; [eglot];
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

    epithet = trivialBuild {
      pname = "epithet";
      version = "20210215.1633";
      src = fetchFromGitHub {
        owner = "oantolin";
        repo = "epithet";
        rev = "cc42338376c2c58da561214c554be8e8856f6a2b";
        hash = "sha256-QJ5kUaTp+poHBfblcKICxlBTVUXeZq5tArU+9qB+ORo=";
      };
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
      packageRequires = with epkgs; [eglot geiser helpful sly];
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
      packageRequires = with epkgs; [quick-peek];
    };

    vundo = trivialBuild {
      pname = "vundo";
      version = "20211107.41";
      src = fetchFromGitHub {
        owner = "casouri";
        repo = "vundo";
        rev = "578a9f6be1a8d4827d05888aa10539989976e696";
        sha256 = "17zdjpp0218h39gy78cy6lqj1lla6nrqnfl9jmzdjpdq1ln8sfmh";
        # date = 2021-11-07T00:41:48-07:00;
      };
    };
  }
