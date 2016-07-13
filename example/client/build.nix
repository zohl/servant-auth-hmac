{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.callPackage ./example.nix { }
