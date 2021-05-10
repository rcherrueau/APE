let
  rust-overlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz");
  pkgs = import <nixpkgs> { overlays = [rust-overlay]; };
  # rust-dev = pkgs.rust-bin.nightly.latest.default.override {
  #   # extensions = [ "cargo" "rust-analyzer" "rustup" ];
  #   targets = [ "x86_64-unknown-linux-musl" ];
  # };
in
  pkgs.mkShell {
    buildInputs = [
      (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
      pkgs.openssl
    ];

    OPENSSL_DIR="${pkgs.openssl.dev}";
    OPENSSL_LIB_DIR="${pkgs.openssl.out}/lib";
    MUSL_TARGET="--target=x86_64-unknown-linux-musl";

    shellHook = ''
    '';
}
