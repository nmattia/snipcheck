let
  pkgs = import ./nix {};
in pkgs.haskellPackages.shellFor
  {
    packages = p: [ p.snipcheck ];
    withHoogle = false;
    buildInputs = [ pkgs.cabal-install ];
  }
