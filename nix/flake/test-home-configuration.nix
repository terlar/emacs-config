{
  self,
  lib,
  config,
  ...
}: {
  flake = {
    homeConfigurations = lib.genAttrs config.systems (system: let
      pkgs = self.legacyPackages.${system};
    in
      self.inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

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
      });
  };

  perSystem = {system, ...}: {
    checks.build-home-configuration = self.homeConfigurations.${system}.activationPackage;
  };
}
