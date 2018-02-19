{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc822
}:

# with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

pkgs.mkShell {
  buildInpupt = [ compiler.stack pkgs.zlib ];
  shellHook = ''
    stack --nix build --copy-compiler-tool ghc-mod
    PATH=$PATH:$(stack --nix path --compiler-tools-bin) 
  '';
}
