let
  pkgs = import <nixpkgs> { };
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.jdk
      pkgs.sbt
      pkgs.metals # lsp
    ];

    shellHook = ''
    '';
}
