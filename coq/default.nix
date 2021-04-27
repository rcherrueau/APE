{ pkgs ? (import <nixpkgs> {})
}:

let
  ocamlInit = pkgs.writeText "ocamlinit" ''
    #use "topfind";;
  '';
in
pkgs.mkShell {
  buildInputs = [ pkgs.coq
                  pkgs.coq.ocaml
                  pkgs.coq.ocamlPackages.utop ];
  shellHook = ''
    alias ocaml="ocaml -init ${ocamlInit}"
    alias utop="utop -init ${ocamlInit}"
  '';
}
