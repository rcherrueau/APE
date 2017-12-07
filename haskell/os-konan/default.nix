with import <nixpkgs> {};

let ghc = haskellPackages.ghcWithHoogle (pkgs: with pkgs; [
  # These are for spacemacs haskell layer. To get spacemacs with the
  # correct PATH. run nix-shell, then launch Emacs inside this
  # nix-shell.
  apply-refact hlint stylish-haskell hasktags ghc-mod
  # apply-refact hlint stylish-haskell hasktags

  #nix-env -qaP -A nixos.haskellPackages|fgrep aeson
  cabal-install 
  aeson aeson-pretty simple-sql-parser
  # pretty-show
  # aeson extra HUnit
]);
in stdenv.mkDerivation {
  name = "os-konan";
  buildInputs = [ ghc jq ];
  shellHook = ''
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}
