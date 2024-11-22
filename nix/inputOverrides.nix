{ lib, ... }:
{
  devdocs = _: prev: {
    packageRequires = builtins.removeAttrs prev.packageRequires [
      # Unused integrations.
      "mathjax"
    ];
  };

  ghelp = _: prev: {
    files = builtins.removeAttrs prev.files [
      # Unused integrations.
      "ghelp-geiser.el"
      "ghelp-lspce.el"
    ];

    packageRequires = prev.packageRequires // {
      # Used integrations.
      sly = "0";
      helpful = "0";
    };
  };

  magit = _: prev: {
    files = prev.files // {
      "lisp/Makefile" = "Makefile";
    };
  };

  org-contrib = _: prev: {
    files = builtins.removeAttrs prev.files [
      # Unused integrations.
      "lisp/ob-stata.el"
    ];
  };

  org-noter = _: prev: {
    packageRequires = prev.packageRequires // {
      pdf-tools = "0";
    };
  };

  rustic = _: prev: {
    files = builtins.removeAttrs prev.files [
      # Unused integrations.
      "rustic-flycheck.el"
    ];
    packageRequires = builtins.removeAttrs prev.packageRequires [
      # Unused integrations.
      "flycheck"
    ];
  };

  pairable = _: _: {
    src = lib.sourceByRegex ../. [
      "lisp"
      "lisp/pairable.el"
    ];
  };

  readable = _: _: {
    src = lib.sourceByRegex ../. [
      "lisp"
      "lisp/readable.el"
    ];
  };

  readable-mono-theme = _: _: {
    src = lib.sourceByRegex ../. [
      "lisp"
      "lisp/readable-mono-theme.el"
    ];
  };

  readable-typo-theme = _: _: {
    src = lib.sourceByRegex ../. [
      "lisp"
      "lisp/readable-typo-theme.el"
    ];
  };
}
