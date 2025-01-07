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
    name = "nongnu";
    type = "elpa";
    path = inputs.nongnu-elpa.outPath + "/elpa-packages";
  }
  {
    name = "gnu-archive";
    type = "archive";
    url = "https://elpa.gnu.org/packages/";
  }
]
