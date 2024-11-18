{ inputs, config, ... }:
let
  rootConfig = config;
in
{
  transposition.homeConfigurations.adHoc = true;

  perSystem =
    { config, pkgs, ... }:
    {
      homeConfigurations = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          rootConfig.flake.homeManagerModules.emacsConfig
          {
            home = {
              stateVersion = "22.05";
              username = "test";
              homeDirectory = "/home/test";
            };

            custom.emacsConfig = {
              enable = true;
              package = config.packages.emacs-env;
              configPackage = config.packages.emacs-config;
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
