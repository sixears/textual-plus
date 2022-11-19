{
  description = "manage info.yaml";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    tfmt.url         = github:sixears/tfmt/r0.2.7.23;
  };

  outputs = { self, nixpkgs, build-utils
            , tfmt }:
    build-utils.lib.hOutputs self nixpkgs "textual-plus" {
      ghc = p: p.ghc8107; # for tfmt
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, data-textual, mtl, text}:
        mkDerivation {
          pname = "textual-plus";
          version = "1.0.2.25";
          src = ./.;
          libraryHaskellDepends = [
            base base-unicode-symbols data-textual mtl text
          ] ++ mapPkg [ tfmt ];
          description = "manage info.yaml";
          license = lib.licenses.mit;
        };
    };
}
