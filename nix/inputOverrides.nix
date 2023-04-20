{lib, ...}: {
  bbdb = _: prev: {
    files =
      prev.files
      // {
        "lisp/bbdb-site.el.in" = "bbdb-site.el.in";
      };
  };

  eval-in-repl = _: prev: {
    # The version specification in eval-in-repl-pkg.el is a placeholder,
    # so specify an actual version instead.
    version = "0.9.7";

    files = builtins.removeAttrs prev.files [
      # Unused integrations.
      "eval-in-repl-geiser.el"
      "eval-in-repl-hy.el"
      "eval-in-repl-iex.el"
      "eval-in-repl-javascript.el"
      "eval-in-repl-slime.el"
      "eval-in-repl-sml.el"
    ];

    packageRequires =
      prev.packageRequires
      // {
        dash = "2";
        paredit = "0";
        ace-window = "0";

        # Used integrations.
        cider = "0";
        elixir-mode = "0";
        elm-mode = "0";
        erlang = "0";
        inf-ruby = "0";
        lua-mode = "0";
        racket-mode = "0";
        sly = "0";
        tuareg = "0";
        vterm = "0";
      };
  };

  ghelp = _: prev: {
    files = builtins.removeAttrs prev.files [
      # Unused integrations.
      "ghelp-geiser.el"
      "ghelp-lspce.el"
    ];

    packageRequires =
      prev.packageRequires
      // {
        # Used integrations.
        sly = "0";
        helpful = "0";
      };
  };

  magit = _: prev: {
    files =
      prev.files
      // {
        "lisp/Makefile" = "Makefile";
      };
  };

  org-babel-eval-in-repl = _: prev: {
    files = builtins.removeAttrs prev.files [
      # Unused integrations.
      "eval-in-repl-ess.el"
    ];
    packageRequires = builtins.removeAttrs prev.packageRequires [
      # Unused integrations.
      "ess"
    ];
  };

  org-noter = _: prev: {
    packageRequires =
      prev.packageRequires
      // {
        pdf-tools = "0";
      };
  };

  rustic = _: prev: {
    files = builtins.removeAttrs prev.files [
      # Unused integrations.
      "rustic-flycheck.el"
    ];
  };

  pairable = _: _: {
    src = lib.sourceByRegex ../. ["lisp" "lisp/pairable.el"];
  };

  readable = _: _: {
    src = lib.sourceByRegex ../. ["lisp" "lisp/readable.el"];
  };

  readable-mono-theme = _: _: {
    src = lib.sourceByRegex ../. ["lisp" "lisp/readable-mono-theme.el"];
  };

  readable-typo-theme = _: _: {
    src = lib.sourceByRegex ../. ["lisp" "lisp/readable-typo-theme.el"];
  };
}
