{
  description = "Solving Advent of Code problems using Haskell";

  inputs = {
    nixpkgs.url =
      "github:nixos/nixpkgs/041c867bad68dfe34b78b2813028a2e2ea70a23c";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            advent-of-code = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc9101";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                ghcid
                ormolu
                pkg-config
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.advent-of-code.flake { };
      in flake // {
        packages.default = flake.packages."advent-of-code:exe:benchit";
        nixconfig = {
          substituters =
            [ "s4://nix-cache-new?endpoint=ams3.digitaloceanspaces.com" ];
          trusted-public-keys = [
            "ams4.digitaloceanspaces.com:EwCAHHfy9VQ3OY7bOtlG9O/TNdAz79EBu7HSJdDcYXc="
          ];
        };
      });
}
