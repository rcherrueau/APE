with import <nixpkgs> {}; {
  pyEnv = stdenv.mkDerivation {
    name = "py";
    buildInputs = [ stdenv python3 python35Packages.virtualenv ];
    shellHook = ''
      if [ ! -d "venv" ]; then
	virtualenv venv
      fi
    '';
  };
}
