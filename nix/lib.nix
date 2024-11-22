{ lib, ... }:

{
  twistPackageSource =
    twistEnv: package:
    lib.pipe twistEnv.packageInputs [
      (lib.filterAttrs (
        _: data:
        lib.pipe data.packageRequires [
          builtins.attrNames
          (builtins.elem package)
        ]
      ))
      builtins.attrNames
    ];
}
