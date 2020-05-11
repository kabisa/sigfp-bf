{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix { } }:

with pkgs;

let
  inherit (haskell.lib) dontCheck doJailbreak;
  haskellPackages = haskell.packages.${compiler};
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      neat-interpolation = dontCheck super.neat-interpolation;
      type-errors-pretty = doJailbreak super.type-errors-pretty;
    };
  };
  source = nix-gitignore.gitignoreSource [ ] ./.;
  drv = haskellPkgs.callCabal2nix "bf" source { };
in {
  bf = drv;
  bf-shell = haskellPkgs.shellFor {
    packages = p: [ drv ];
    buildInputs = with haskellPkgs; [
      cabal-install
      hpack
      hlint
      ghcid
    ];
    withHoogle = true;
  };
}
