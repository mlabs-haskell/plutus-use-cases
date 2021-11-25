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
    let
      inherit (flake-utils.lib) defaultSystems;

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system: import nixpkgs {
        overlays = [ haskellNix.overlay ];
        inherit (haskellNix) config;
        inherit system;
      };

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plutus = import plutusSrc { inherit system; };
        in
          import ./nix/haskell.nix {
            inherit system pkgs plutus;
          };

    in
      {
        flake = perSystem (system: (projectFor system).flake {});

        defaultPackage = perSystem (
          system:
            self.flake.${system}.packages."mlabs-plutus-use-cases:lib:mlabs-plutus-use-cases"
        );

        packages = perSystem (system: self.flake.${system}.packages);

        apps = perSystem (system: self.flake.${system}.apps);

        devShell = perSystem (system: self.flake.${system}.devShell);

        # NOTE `nix flake check` will not work at the moment due to use of
        # IFD in haskell.nix
        checks = perSystem (
          system:
            let
              flakePkgs = self.flake.${system}.packages;
            in
              {
                tests =
                  flakePkgs."mlabs-plutus-use-cases:test:mlabs-plutus-use-cases-tests";
                deploy-app =
                  flakePkgs."mlabs-plutus-use-cases:exe:deploy-app";
                governance-demo =
                  flakePkgs."mlabs-plutus-use-cases:exe:governance-demo";
                lendex-demo =
                  flakePkgs."mlabs-plutus-use-cases:exe:lendex-demo";
                mlabs-plutus-use-cases =
                  flakePkgs."mlabs-plutus-use-cases:exe:mlabs-plutus-use-cases";
                nft-demo = flakePkgs."mlabs-plutus-use-cases:exe:nft-demo";
                nft-marketplace =
                  flakePkgs."mlabs-plutus-use-cases:exe:nft-marketplace";
              }
        );

      };
}