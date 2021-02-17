{ makeDesktopItem, symlinkJoin, writeShellScriptBin }:
let
  emacseditorScript = writeShellScriptBin "emacseditor" ''
    if [ -z "$1" ]; then
      exec emacsclient --create-frame --alternate-editor emacs
    else
      exec emacsclient --alternate-editor emacs "$@"
    fi
  '';
  emacseditorDesktopItem = makeDesktopItem {
    name = "emacseditor";
    exec = "emacseditor %F";
    icon = "emacs";
    comment = "Edit text";
    desktopName = "Emacs Editor";
    genericName = "Text Editor";
    categories = "Development;TextEditor;";
    mimeType =
      "text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;application/pdf;";
    extraEntries = ''
      StartupWMClass=Emacs
      Keywords=Text;Editor;
    '';
  };

  emacsmailScript = writeShellScriptBin "emacsmail" ''
    exec emacsclient --create-frame --eval "(browse-url-mail \"$@\")"
  '';

  emacsmailDesktopItem = makeDesktopItem {
    name = "emacsmail";
    exec = "emacsmail %u";
    icon = "emacs";
    comment = "Mail/News Client";
    desktopName = "Emacs Mail";
    genericName = "Mail/News Client";
    mimeType = "x-scheme-handler/mailto;";
  };
in
symlinkJoin {
  name = "emacs-utils";
  paths = [
    emacseditorScript
    emacseditorDesktopItem
    emacsmailScript
    emacsmailDesktopItem
  ];
}
