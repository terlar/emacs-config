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
            nixpkgs.overlays = [ rootConfig.flake.overlays.default ];

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
