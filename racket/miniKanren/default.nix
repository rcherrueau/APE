{ pkgs ? (import <nixpkgs> {})
, racket2nix ? (import (fetchTarball {
    url = https://github.com/fractalide/racket2nix/archive/5627ccfd9951073f2b33e9b9fff02d85647c3edb.tar.gz;
    sha256 = "0yvgwj18y194bg0q357xydqb2751iqki0yn9xbrvwiw4a0rqkr5r";
  }) {})
}:

# let cKanren = racket2nix.buildRacketPackage "git://github.com/calvis/cKanren.git#master";
# in  
pkgs.mkShell {
  # buildInputs = [ pkgs.racket cKanren ];
  buildInputs = [ pkgs.racket ];
  shellHook = ''
    echo 'Run: raco pkg install "git://github.com/calvis/cKanren.git?path=cKanren#master"'
  '';
}
