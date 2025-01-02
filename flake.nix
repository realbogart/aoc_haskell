{
  description = "Solving Advent of Code problems using Haskell";

  inputs = {
    nixpkgs.url =
      "github:nixos/nixpkgs/e12483116b3b51a185a33a272bf351e357ba9a99";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            benchit = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc98";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [ nixpkgs-fmt ghcid ormolu ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.benchit.flake { };
      in flake // {
        packages.default = flake.packages."benchit:exe:benchit";
        nixconfig = {
          substituters =
            [ "s4://nix-cache-new?endpoint=ams3.digitaloceanspaces.com" ];
          trusted-public-keys = [
            "ams4.digitaloceanspaces.com:EwCAHHfy9VQ3OY7bOtlG9O/TNdAz79EBu7HSJdDcYXc="
          ];
        };
      });
}
