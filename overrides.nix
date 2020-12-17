{ stdenv, fetchFromGitHub, substituteAll, texinfo, perl, python3, pywal, which
, gcc, pkg-config, glib-networking, gtk3, webkitgtk }:

epkgs:

let inherit (epkgs) trivialBuild;
in epkgs // {
  # Patched.
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

  # Packages not in MELPA/GNU ELPA.
  apheleia = trivialBuild rec {
    pname = "apheleia";
    version = "20201107.704";
    src = fetchFromGitHub {
      owner = "raxod502";
      repo = "apheleia";
      rev = "8a1e68441ca418c2a277d7aef663790f26208dd8";
      sha256 = "15hgy98d7fxjl6rfwvrhq1sx9hbg31bh1j2baq84km6zzds9bh5p";
      # date = 2020-11-07T07:04:41-08:00;
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
    version = "20201208.1659";
    src = fetchFromGitHub {
      owner = "nyyManni";
      repo = "ejira";
      rev = "8675aaef6f8774f2c8aa6cf0957aba012743760b";
      sha256 = "03xv75b143rblpslrwynpqd9ypzmmw5w007ncr14jcbzkw4n6amg";
      # date = 2020-12-08T16:59:33+02:00;
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

  embark = trivialBuild {
    pname = "embark";
    version = "20201216.1535";
    src = fetchFromGitHub {
      owner = "oantolin";
      repo = "embark";
      rev = "8ebafd0bd2746f0776cfb600c1f2f2c359c9d038";
      sha256 = "1f9g114s9w7r3lg3asyks571rh2fzanlw35v426wmhi0w4pgr5pr";
      # date = 2020-12-16T15:35:14-06:00;
    };
    packageRequires = with epkgs; [ avy ];
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
    version = "20201126.1452";
    src = fetchFromGitHub {
      owner = "casouri";
      repo = "ghelp";
      rev = "e54ad2434d8a30e5304c0c311eeee05e09802ba9";
      sha256 = "1rn6g3g3zwsab8mnf52k7cibahprjn2hyj04nnq2b66zdsadvrr0";
      # date = 2020-11-26T14:52:16-05:00;
    };
    packageRequires = with epkgs; [ eglot geiser helpful ];
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
    version = "20201128.1605";
    src = fetchFromGitHub {
      owner = "mickeynp";
      repo = "ligature.el";
      rev = "c830b9d74dcf4ff08e6f19cc631d924ce47e2600";
      sha256 = "1a48h3fj0vs7abashwz3shld724abin7a41vilf5mjzapry9fmkh";
      # date = 2020-11-28T16:05:36+00:00;
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

  valign = trivialBuild {
    pname = "valign";
    version = "20201216.1137";
    src = fetchFromGitHub {
      owner = "casouri";
      repo = "valign";
      rev = "740d5072190ebc7dd75a1ae8c4f8d3cb99faa4fe";
      sha256 = "13n73fwy8xfhnh4pjmhprxib1r0f82vmbxk4haff85a0zhy1h870";
      # date = 2020-12-16T11:37:01-05:00;
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
