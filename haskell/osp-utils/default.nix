with import <nixpkgs> {}; {
_ = let ghc = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
  # These are for spacemacs haskell layer. To get spacemacs with the
  # correct PATH. run nix-shell, then launch Emacs inside this
  # nix-shell.
  apply-refact hlint stylish-haskell

  #nix-env -qaP -A nixos.haskellPackages|fgrep aeson
  cabal-install aeson extra HUnit
]);
in stdenv.mkDerivation {
  name = "osp-utils";
  buildInputs = [ ghc python27Full python27Packages.seqdiag ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
};
}
