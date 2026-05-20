{ pkgs, ... }:

{
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.python3
    pkgs.stack
  ];

  scripts.build.exec = "cabal v2-build";

  scripts.smoke.exec = "scripts/gtp-regression";

  enterShell = ''
    echo "kurt dev shell: use 'build' or 'smoke'"
  '';
}
