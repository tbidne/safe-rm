{
  description = "A simple utility for deleting files";
  inputs.algebra-simple-src.url = "github:tbidne/algebra-simple";
  inputs.byte-types-src.url = "github:tbidne/byte-types";
  inputs.classy-containers-src.url = "github:tbidne/classy-containers";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    { algebra-simple-src
    , byte-types-src
    , classy-containers-src
    , flake-compat
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
          name = "safe-rm";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv
              (buildTools compiler ++
                (if withDevTools then devTools compiler else [ ]));
          overrides = final: prev: with compiler; {
            algebra-simple =
              pkgs.haskell.lib.doJailbreak
                (final.callCabal2nix "algebra-simple" algebra-simple-src { });
            byte-types =
              pkgs.haskell.lib.doJailbreak
                (final.callCabal2nix "byte-types" byte-types-src { });
            classy-containers =
              pkgs.haskell.lib.doJailbreak
                (final.callCabal2nix "classy-containers" classy-containers-src { });
            package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
            tasty-hedgehog = prev.tasty-hedgehog_1_3_0_0;
          };
        };
    in
    {
      packages.default = mkPkg false false;

      devShells.default = mkPkg true true;
      devShells.ci = mkPkg true false;
    });
}
