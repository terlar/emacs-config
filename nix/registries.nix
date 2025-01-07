{ inputs }:
[
  {
    name = "custom";
    type = "melpa";
    path = ../recipes;
  }
  {
    name = "melpa";
    type = "melpa";
    path = inputs.melpa.outPath + "/recipes";
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
