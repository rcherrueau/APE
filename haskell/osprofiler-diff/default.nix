with import <nixpkgs> {}; {
_ = let ghc = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
  apply-refact hlint stylish-haskell # These are for spacemacs haskell
                                     # layer. To get spacemacs with
                                     # the correct PATH. run
                                     # nix-shell, then launch emacs
                                     # inside this nix-shell.

  #nix-env -qaP -A nixos.haskellPackages|fgrep aeson
  aeson
]);
in stdenv.mkDerivation {
  name = "osprofiler-diff";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
};
}
