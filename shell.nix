let
  pkgs = import <nixpkgs> {};

  # Wrapped stack executable that uses the nix-provided GHC
  stack = pkgs.stdenv.mkDerivation {
      name = "stack-system-ghc";
      builder = pkgs.writeScript "stack" ''
        source $stdenv/setup
        mkdir -p $out/bin
        makeWrapper ${pkgs.stack}/bin/stack $out/bin/stack \
          --add-flags --system-ghc  \
          --add-flags "--extra-lib-dirs ${pkgs.zlib}/lib"
      '';
      buildInputs = [ pkgs.makeWrapper ];
    };
  ghc = pkgs.haskell.compiler.ghc822;

in pkgs.mkShell
  { shellHook = "export LD_LIBRARY_PATH=${pkgs.zlib}/lib";
    buildInputs = [ stack ghc pkgs.zlib ];
  }
