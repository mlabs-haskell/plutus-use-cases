let
  srcs = import ./nix/sources.nix;
  pkgs = import srcs.nixpkgs { };
  plutusRepo = "https://github.com/input-output-hk/plutus";
  nivPins = builtins.fromJSON (builtins.readFile ./nix/sources.json);
  plutus = builtins.fetchGit {
    url = plutusRepo;
    rev = nivPins.plutus.rev;
  };
  plutusShell = import "${plutus}/shell.nix" { };
  newShell = plutusShell.overrideAttrs (oldAttr:
    oldAttr // rec {

      # Add any extra packages you want in the environment here
      extraInputs = with pkgs; [ nixfmt graphviz ];

      buildInputs = oldAttr.buildInputs ++ extraInputs;

      shellHook = ''
        echo "***-----------------***"
        echo "*** MLabs Dev-Shell ***"
        echo "***-----------------***"
      '';
    });
in
newShell
