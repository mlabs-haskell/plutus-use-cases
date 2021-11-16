{
  inputs = {
    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
    };

    # The Plutus "flake" isn't really a flake - it doesn't define any
    # outputs and is only used for input pinning according to to
    # the comments
    plutusSrc = {
      type = "github";
      owner = "input-output-hk";
      repo = "plutus";
      rev = "3f089ccf0ca746b399c99afe51e063b0640af547";
      flake = false;
    };

    haskellNix = {
      type = "github";
      owner = "input-output-hk";
      repo = "haskell.nix";
      # FIXME rev taken from Plutus doesn't work?
      rev = "64cd5f70ce0d619390039a1a3d57c442552b0924";
    };

    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    flake-compat = {
      type = "github";
      owner = "edolstra";
      repo = "flake-compat";
      rev = "12c64ca55c1014cdc1b16ed5a804aa8576601ff2";
      flake = false;
    };
  };

  outputs = { self, flake-utils, plutusSrc, nixpkgs, haskellNix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (
        system:
          let
            plutus = import plutusSrc { inherit system; };

            overlays = [
              haskellNix.overlay
              (
                final: _: {
                  mlabs-plutus-use-cases = import ./nix/haskell.nix {
                    inherit final pkgs plutus;
                  };
                }
              )
            ];

            pkgs = import nixpkgs {
              inherit system overlays;
              inherit (haskellNix) config;
            };

            flake = pkgs.mlabs-plutus-use-cases.flake {};

          in
            flake // {
              defaultPackage =
                flake.packages."mlabs-plutus-use-cases:lib:mlabs-plutus-use-cases";

              checks = flake.checks // {
                deploy-app =
                  flake.packages."mlabs-plutus-use-cases:exe:deploy-app";
                governance-demo =
                  flake.packages."mlabs-plutus-use-cases:exe:governance-demo";
                lendex-demo =
                  flake.packages."mlabs-plutus-use-cases:exe:lendex-demo";
                mlabs-plutus-use-cases =
                  flake.packages."mlabs-plutus-use-cases:exe:mlabs-plutus-use-cases";
                nft-demo = flake.packages."mlabs-plutus-use-cases:exe:nft-demo";
                nft-marketplace =
                  flake.packages."mlabs-plutus-use-cases:exe:nft-marketplace";
              };

            }
      );
}
