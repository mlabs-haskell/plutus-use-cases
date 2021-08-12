let
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs {};
  pkgs_master = import sources.nixpkgs_master {};
  easy-ps = import sources.easy-purescript-nix {};
in

pkgs.mkShell {
  buildInputs = builtins.attrValues {
    inherit (pkgs) gnumake nodejs;
    inherit (pkgs_master.nodePackages) purescript-language-server;
    inherit (easy-ps) purs pulp purp psc-package dhall-simple spago psa pscid spago2nix zephyr;
  };
}
