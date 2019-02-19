{ haskellPackages
, haskell
, lib
}:
with
{ localHaskellPakcages = haskellPackages.extend
    (haskell.lib.packageSourceOverrides
      { snipcheck = lib.cleanSource ../.;
      }
    );
};
{ snipcheck = localHaskellPakcages.snipcheck;
}
