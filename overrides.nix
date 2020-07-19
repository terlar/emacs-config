{ stdenv, fetchFromGitHub, writeText, texinfo, perl, which }:

self: super:

with super;

{
  # Forks.
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

  ws-butler = ws-butler.overrideAttrs (attrs: {
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
    packageRequires =
      [ dash-functional f helm jiralib2 language-detection org ox-jira s ];
  };

  explain-pause-mode = trivialBuild {
    pname = "explain-pause-mode";
    version = "20200718.1853";
    src = fetchFromGitHub {
      owner = "lastquestion";
      repo = "explain-pause-mode";
      rev = "02961514aa1a102691be4ad5c0a90283920621ca";
      sha256 = "1jdgx05vnrscq1ic3mkw88h23k3nazxansykw045x1rvzyd8qrbl";
      # date = 2020-07-18T18:53:54-07:00;
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
    version = "20200711.2339";
    src = fetchFromGitHub {
      owner = "casouri";
      repo = "valign";
      rev = "eaa5cb11add73fa90d3333261a2fc3f75d676018";
      sha256 = "056bpjd2nq3z95y0ypdplmpr7gyydnb7ssd5khn21ns5rfwg1sql";
      # date = 2020-07-11T23:39:16-04:00;
    };
  };
}
