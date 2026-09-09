{ pkgs, ... }:

{
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.python3
  ];

  scripts.build.exec = "cabal v2-build";

  scripts.test.exec = "cabal v2-test";

  scripts.smoke.exec = "scripts/gtp-regression";
}
