{ stdenv, fetchFromGitHub, substituteAll, texinfo, perl, python3, pywal, which
}:

final: prev:

let inherit (prev) trivialBuild;
in {
  # Forks.
  all-the-icons-dired = prev.all-the-icons-dired.overrideAttrs (attrs: {
    version = "20200827.1545";
    src = fetchFromGitHub {
      owner = "terlar";
      repo = "all-the-icons-dired";
      rev = "b06bb30c79d8e1b13b9214bf38d1e1d837de4dd3";
      sha256 = "1fwb47gwrjd5vdnjkrycp14a0drfaysyq1vxv1krydmy17s9r3lv";
      # date = 2020-08-27T15:45:50+02:00;
    };
  });

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

  org = stdenv.mkDerivation rec {
    pname = "org";
    version = "20200825.2339";

    src = fetchFromGitHub {
      owner = "terlar";
      repo = "org-mode";
      rev = "053a82fc6d6ce27d2e165d608a89bb2086768c91";
      sha256 = "080vdln3vh7hvn42c9cyy5126393bwdybxdib1b59q8aaqgr2im8";
      # date = 2020-08-25T23:39:28+02:00;
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

    buildInputs = with final; [ emacs texinfo perl which ];

    meta = with stdenv.lib; {
      homepage = "https://elpa.gnu.org/packages/org.html";
      license = licenses.free;
    };
  };

  org-variable-pitch = prev.org-variable-pitch.overrideAttrs (attrs: {
    version = "20200828.230";
    src = fetchFromGitHub {
      owner = "terlar";
      repo = "elisp";
      rev = "274a897e999334e86db36fc0f0a0c7fb3d833030";
      sha256 = "1dfv5f8w1b2gyii0yznx417a4l7pip3510l377ymqy4qhxajs7pw";
      # date = 2020-08-28T02:30:27+02:00;
    };
  });

  theme-magic = prev.theme-magic.overrideAttrs (attrs: {
    version = "20200828.230";
    patches = [
      (substituteAll {
        src = ./patches/theme-magic.patch;
        python = "${python3}/bin/python";
        wal = "${pywal}/bin/wal";
      })
    ];
  });

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
    version = "20200716.826";
    src = fetchFromGitHub {
      owner = "raxod502";
      repo = "apheleia";
      rev = "6aff83d5acca936cf1103e0dbe59724c4e887560";
      sha256 = "112pqvmhh484d4b4wrawz0x9gl6x9c36rmqwym7masxk7w6hdm97";
      # date = 2020-07-16T08:26:51-06:00;
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

  find-file-rg = trivialBuild {
    pname = "find-file-rg";
    version = "20200820.1226";
    src = fetchFromGitHub {
      owner = "muffinmad";
      repo = "emacs-find-file-rg";
      rev = "a690ce283fecd353240b269350216ae631f5352d";
      sha256 = "1wp6issswh1c0i25ayh628vqpi1ixs47wn7dhk0m5786va7mmrm5";
      # date = 2020-08-20T12:26:39+03:00;
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
