{
  self,
  emacsSrc,
}: [
  {
    name = "custom";
    type = "melpa";
    path = ../recipes;
  }
  {
    name = "gnu";
    type = "elpa";
    path = self.inputs.gnu-elpa.outPath + "/elpa-packages";
    core-src = emacsSrc;
    auto-sync-only = true;
  }
  {
    name = "melpa";
    type = "melpa";
    path = self.inputs.melpa.outPath + "/recipes";
  }
  {
    name = "nongnu";
    type = "elpa";
    path = self.inputs.nongnu-elpa.outPath + "/elpa-packages";
  }
  {
    name = "gnu-archive";
    type = "archive";
    url = "https://elpa.gnu.org/packages/";
  }
]
