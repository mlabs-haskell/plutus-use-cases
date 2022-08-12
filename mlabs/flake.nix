{
  description = "mlabs-plutus-use-cases";

  inputs = {
    haskell-nix.url = "github:L-as/haskell.nix/45c583b5580c130487eb5a342679f0bdbc2b23fc";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    nixpkgs.follows = "haskell-nix/nixpkgs";

    iohk-nix.url = "github:input-output-hk/iohk-nix";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/71006f9eb956b0004022e80aadd4ad50d837b621";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/41545ba3ac6b3095966316a99883d678b5ab8da8";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5";
      flake = false;
    };
    cardano-node = {
      url = "github:input-output-hk/cardano-node/814df2c146f5d56f8c35a681fe75e85b905aed5d";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    cardano-wallet = {
      url =
        "github:input-output-hk/cardano-wallet/a5085acbd2670c24251cf8d76a4e83c77a2679ba";
      flake = false;
    };
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/46f994e216a1f8b36fe4669b47b2a7011b0e153c";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/d2d219a86cda42787325bb8c20539a75c2667132";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/1a3c3a761cf048371c52a34b004f8b3fcf0dab43";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:t4ccer/plutus-apps/82c0725c4d05398ae76d71927cc60aa23db1a11d";
      flake = false;
    };
    plutus-extra = {
      url =
        "github:Liqwid-Labs/plutus-extra/4722305495c8c4b03ff06debf0f4a041768a5467";
      flake = false;
    };
    plutus-tx-spooky = {
      url =
        "gitlab:fresheyeball/plutus-tx-spooky/0c409907fa5b6aee4a2f2d18f871b850a8547fdf";
      flake = false;
    };
    # plutus-simple-model = {
    #   url =
    #     "github:t4ccer/plutus-simple-model/48c186f96e3a8a07bceb1a4b39a7dfeacddde42b";
    #   flake = false;
    # };
    purescript-bridge = {
      url =
        "github:input-output-hk/purescript-bridge/47a1f11825a0f9445e0f98792f79172efef66c00";
      flake = false;
    };
    servant-purescript = {
      url =
        "github:input-output-hk/servant-purescript/44e7cacf109f84984cd99cd3faf185d161826963";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
    # plutip = {
    #   url = "github:mlabs-haskell/plutip/c2d0ed381cda64bc46dbf68f52cb0d05f76f3a86";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.haskell-nix.follows = "haskell-nix";
    #   inputs.cardano-node.follows = "cardano-node";
    # };
    bot-plutus-interface = {
      url = "github:mlabs-haskell/bot-plutus-interface/bdb6f74b2616b637fcba633b46e2f75f68fc7a11";
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay iohk-nix.overlays.crypto ];
          inherit (haskell-nix) config;
          inherit system;
        };

      nixpkgsFor' = system: import nixpkgs { inherit system; };

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plutus = import inputs.plutus { inherit system; };
          src = ./.;
        in import ./nix/haskell.nix { inherit src inputs pkgs system; };

    in {
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "mlabs-plutus-use-cases:lib:mlabs-plutus-use-cases";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check" {
          nativeBuildInputs = builtins.attrValues self.checks.${system}
            ++ builtins.attrValues self.flake.${system}.packages
            ++ [ self.flake.${system}.devShell.inputDerivation ];
        } "touch $out");

      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      #
      # Includes all of the packages in the `checks`, otherwise only the
      # test suite would be included
      checks = perSystem (system: self.flake.${system}.checks);
      hydraJobs.checks.x86_64-linux = self.checks.x86_64-linux;
    };
}
