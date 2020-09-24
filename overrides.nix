{ stdenv, fetchFromGitHub, substituteAll, texinfo, perl, python3, pywal, which
}:

final: prev:

let inherit (prev) trivialBuild;
in {
  # Patched.
  theme-magic = prev.theme-magic.overrideAttrs (attrs: {
    patches = [
      (substituteAll {
        src = ./patches/theme-magic.patch;
        python = "${python3}/bin/python";
        wal = "${pywal}/bin/wal";
      })
    ];
  });

  # Forks.
  flymake-diagnostic-at-point = prev.flymake-diagnostic-at-point.overrideAttrs
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
    packageRequires = with final; [ dash f s ];
  };

  ws-butler = prev.ws-butler.overrideAttrs (attrs: {
    version = "20200403.107";
    src = fetchFromGitHub {
      owner = "hlissner";
      repo = "ws-butler";
      rev = "2bb49d3ee7d2cba133bc7e9cdac416cd1c5e4fe0";
      sha256 = "1ifrcxlb6hinjv4bn54c8fars4avcm5ijaj44h606mqymj37dvn1";
      # date = 2020-04-03T01:07:46-04:00;
    };
  });

  # Packages not in MELPA/GNU ELPA.
  apheleia = trivialBuild rec {
    pname = "apheleia";
    version = "20200922.630";
    src = fetchFromGitHub {
      owner = "raxod502";
      repo = "apheleia";
      rev = "38488e0ef9d75189076f76be295144353d25c58c";
      sha256 = "11wm7lb0lasz355lpqyyhp94p9nfmakfz7vypsrys0pybr3isd9x";
      # date = 2020-09-22T06:30:42-07:00;
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
    packageRequires = with final; [ eglot ];
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
    packageRequires = with final; [
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

  ligature = trivialBuild {
    pname = "ligature";
    version = "20200901.1702";
    src = fetchFromGitHub {
      owner = "mickeynp";
      repo = "ligature.el";
      rev = "9aa31e7e31286e607c9d4f2d8a2855d4bbf9da9e";
      sha256 = "1lv8yxab5lcjk0m4f27p76hmp17yy9803zhvqpqgh46l05kahjvk";
      # date = 2020-09-01T17:02:24+01:00;
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
    packageRequires = with final; [ quick-peek ];
  };

  valign = trivialBuild {
    pname = "valign";
    version = "20200806.1216";
    src = fetchFromGitHub {
      owner = "casouri";
      repo = "valign";
      rev = "a68750acee4df417e4d13c4c5ede149dcb377cf9";
      sha256 = "0c7p7hsj2gf256kwf7bgc5j23wgsr2gs3dgq93gfmzcivgylbimy";
      # date = 2020-08-06T12:16:35-04:00;
    };
  };
}
