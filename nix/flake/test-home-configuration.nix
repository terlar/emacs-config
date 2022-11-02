{self, ...}: {
  transposition.homeConfigurations.adHoc = true;

  perSystem = {
    config,
    pkgs,
    ...
  }: {
    homeConfigurations = self.inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = config.legacyPackages;

      modules = [
        self.homeManagerModules.emacsConfig
        {
          home = {
            stateVersion = "22.05";
            username = "test";
            homeDirectory = "/home/test";
          };

          custom.emacsConfig = {
            enable = true;
            erc = pkgs.writeText "ercrc.el" ''
              ;; Testing testing
            '';
          };
        }
      ];
    };

    checks.build-home-configuration = config.homeConfigurations.activationPackage;
  };
}
