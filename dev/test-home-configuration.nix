toplevel@{ self, ... }:
let
  inherit (toplevel.config.dev) rootFlake;
in
{
  transposition.homeConfigurations.adHoc = true;

  perSystem =
    { config, pkgs, ... }:
    {
      homeConfigurations = self.inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
          rootFlake.homeManagerModules.emacsConfig
          {
            nixpkgs.overlays = [ rootFlake.overlays.default ];

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
