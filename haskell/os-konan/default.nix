{ pkgs ? (import <nixpkgs> {})
, compiler ? pkgs.haskell.packages.ghc822
}:

# with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

pkgs.mkShell {
  buildInpupt = [ compiler.stack pkgs.zlib ];
  shellHook = ''
    alias stack="stack --nix"
    stack build --copy-compiler-tool ghc-mod
    PATH=$PATH:$(stack path --compiler-tools-bin)

    echo ""
    echo "Command 'stack' is an alias for 'stack --nix'."
    echo "Run 'stack build --test :os-konan-test' for tests."
    echo "Run 'stack ide targets' for the list of available targets."
  '';
}
