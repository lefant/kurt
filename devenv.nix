{ pkgs, ... }:

{
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.python3
    pkgs.stack
  ];

  scripts.build.exec = "cabal v2-build";

  scripts.test.exec = "cabal v2-test";

  scripts.smoke.exec = "scripts/gtp-regression";

  enterShell = ''
    echo "kurt dev shell: use 'build', 'test', or 'smoke'"
  '';
}
