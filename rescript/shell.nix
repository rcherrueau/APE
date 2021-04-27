{ pkgs ? (import <nixpkgs> {}) }:

with pkgs.lib;

let 
  bs-ninja = lists.findFirst (drv: strings.getName(drv) == "ninja") {} pkgs.bs-platform.buildInputs;
  bs-python = lists.findFirst (drv: strings.getName(drv) == "python3") {} pkgs.bs-platform.buildInputs;
in 
  pkgs.mkShell {
    buildInputs = [pkgs.nodejs pkgs.bs-platform pkgs.python3  bs-ninja];
  }
