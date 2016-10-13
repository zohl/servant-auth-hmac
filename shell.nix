{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: {
      cryptonite = self.callPackage ./libs/cryptonite-0.20.nix {};
      # hspec-expectations = self.callPackage ./libs/hspec-expectations-0.8.0.nix {};
      # hspec-wai = self.callPackage ./libs/hspec-wai-0.8.0.nix {};
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

in
  if pkgs.lib.inNixShell then drv.env else drv
