# Waiting the merge of: https://github.com/NixOS/nixpkgs/pull/45448
# See, pinning nixpkgs:
# - https://nixos.org/nix/manual/#ssec-derivation (search for fetchTarball)
# - https://vaibhavsagar.com/blog/2018/05/27/quick-easy-nixpkgs-pinning/
{ pkgs ? (import <nixpkgs> {}) }:

pkgs.mkShell {
  buildInputs = [
    pkgs.zip pkgs.unzip
    pkgs.libreoffice
    pkgs.nodejs-10_x
    pkgs.imagemagickBig
  ];
  shellHook = ''
  '';
}
