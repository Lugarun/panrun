
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in rec {
          packages.panrun = pkgs.haskellPackages.developPackage {
            root = ./.;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [ cabal-install
                ghcid
                haskell-language-server
                pkgs.zlib
              ]);
          };
          packages.default = packages.panrun;
          overlays.default = final: prev: {
            panrun = self.packages.panrun;
          };
          devShells.default = packages.default.env;
        }
    );
}
