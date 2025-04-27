{ rootPath, melpaSrc }:

[
  {
    name = "custom";
    type = "melpa";
    path = rootPath + "/recipes";
  }
  {
    name = "melpa";
    type = "melpa";
    path = melpaSrc + "/recipes";
  }
  {
    name = "gnu-archive";
    type = "archive";
    url = "https://elpa.gnu.org/packages/";
  }
  {
    name = "nongnu-archive";
    type = "archive";
    url = "https://elpa.nongnu.org/nongnu/";
  }
]
