# with import <nixpkgs> {};

# stdenv.mkDerivation {
#   name = "pgdump2cockroach";
#   buildInputs = [ rustStable.rustc rustStable.cargo ];
#   shellHook = ''
#     # eval $(egrep ^export ${ghc}/bin/ghc)
#   '';
# }
{ pkgs ? (import <nixpkgs> {})
}:

pkgs.mkShell {
  buildInputs = [
    # pkgs.rustc pkgs.cargo pkgs.rustfmt pkgs.rustracer
    pkgs.rustup pkgs.rustfmt pkgs.gdb pkgs.gdbgui
  ];

  shellHook = ''
    rustup install stable
    rustup default stable
    rustup update
    rustup component add rls rust-analysis rust-src
  '';
}
