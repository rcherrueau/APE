{ pkgs ? (import <nixpkgs> {}) }:

with pkgs.lib;

let
  # https://github.com/rescript-lang/rescript-vscode
  rescript-lsp = pkgs.fetchzip rec {
    name = "rescript-vscode-1.1.2";
    url = "https://github.com/rescript-lang/rescript-vscode/releases/download/1.1.2/${name}.vsix";
    sha256 = "1xniz8qz7ccdaqvrajpmmgrjq8xzl5jd8r4acpbdab2jbgwxa9cv";
    # Changing this will *not* be take into account if this derivation
    # has already been evaluated successfully.  You need to remove it
    # manually from the nix store first:
    # sudo nix-store --delete --ignore-liveness /nix/store/<path-to-rescripte-vscode-1.1.2-out>
    postFetch = ''
        unzip $downloadedFile -d $out

        # Patch binary
        # https://nixos.wiki/wiki/Packaging/Binaries
        patchelf \
            --set-interpreter "$(cat ${pkgs.gcc}/nix-support/dynamic-linker)" \
            "$out/extension/server/analysis_binaries/linux/rescript-editor-analysis.exe"
    '';
  };
  # Set emacs variable `lsp-rescript-server-command' with the path of
  # `server.js` from rescript-vscode.
  # https://github.com/jjlee/rescript-mode
  emacs-lsp-rescript-server-cmd = ''
    (customize-set-variable
      'lsp-rescript-server-command
      '("${pkgs.nodejs}/bin/node" "${rescript-lsp}/extension/server/out/server.js" "--stdio"))
  '';
  emacs-wrapper = pkgs.writeShellScriptBin "emacs" ''
    ${pkgs.emacs}/bin/emacs --eval=${strings.escapeShellArg emacs-lsp-rescript-server-cmd} "$@"
  '';
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.nodejs pkgs.python3Minimal
      emacs-wrapper
      pkgs.opam
      pkgs.stdenv.cc.cc
    ];

    shellHook = ''
       echo "rescript-vscode path: "
       echo ${rescript-lsp}

       # For patchelf of ninja.exe in rescritp-compiler
       echo "libstdc++.so.6: "
       echo ${pkgs.stdenv.cc.cc.lib}/lib
    '';
  }
