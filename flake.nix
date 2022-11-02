{
  description = "manage info.yaml";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.3";

    tfmt.url         = "github:sixears/tfmt/r0.2.7.3";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils
            , tfmt }:
    build-utils.lib.hOutputs self nixpkgs "textual-plus" {
      deps = {
        inherit tfmt;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
