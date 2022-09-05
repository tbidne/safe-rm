{
  description = "A simple utility for deleting files";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    { flake-compat
    , flake-utils
    , nixpkgs
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      buildTools = c: with c; [
        cabal-install
        pkgs.gnumake
        pkgs.zlib
      ];
      devTools = c: with c; [
        ghcid
        haskell-language-server
      ];
      ghc-version = "ghc924";
      compiler = pkgs.haskell.packages."${ghc-version}";
      mkPkg = returnShellEnv: withDevTools:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "del";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv
              (buildTools compiler ++
                (if withDevTools then devTools compiler else [ ]));
        };
    in
    {
      packages.default = mkPkg false false;

      devShells.default = mkPkg true true;
      devShells.ci = mkPkg true false;
    });
}
