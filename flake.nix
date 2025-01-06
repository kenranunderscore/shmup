{
  description = "A very basic Haskell project flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];

      perSystem =
        { system, ... }:
        let
          ghc = "ghc910";
          pkgs = import nixpkgs {
            inherit system;
            config.allowBroken = true;
            overlays = [
              (final: prev: {
                hspkgs = prev.haskell.packages.${ghc}.override (old: {
                  overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
                    hfinal: hprev: {
                      shmup = hfinal.callCabal2nix "shmup" (final.lib.cleanSource ./.) { };
                      h-raylib = pkgs.haskell.lib.compose.doJailbreak hprev.h-raylib;
                    }
                  );
                });
              })
            ];
          };
        in
        {
          packages.default = pkgs.hspkgs.shmup;

          devShells.default = pkgs.hspkgs.shellFor {
            packages = p: [ p.shmup ];
            nativeBuildInputs = [
              pkgs.hspkgs.cabal-install
              pkgs.hspkgs.cabal-fmt
              pkgs.hspkgs.haskell-language-server
            ];
          };
        };
    };
}
