let
  pkgs = import ((import ../default.nix).inputs.nixpkgs) { };
  compiler = pkgs.haskell.packages."ghc924";
in
pkgs.haskell.lib.buildStackProject {
  name = "del";

  buildInputs = with pkgs; [
    git
    stack
    zlib.dev
    zlib.out
  ];

  ghc = compiler.ghc;

  STACK_YAML = "stack.yaml";
}
