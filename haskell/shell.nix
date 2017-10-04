{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
