{ pkgs, ... }:

{
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.stack
  ];

  scripts.build.exec = "cabal v2-build";

  scripts.smoke.exec = ''
    set +e
    printf 'name\nprotocol_version\nboardsize 5\nkomi 0\nkurt_configure maxplayouts 5\nkurt_configure maxtime 50\nclear_board\nplay b A1\ngenmove w\nquit\n' \
      | cabal v2-run kurt -- +RTS -N1 > /tmp/kurt-smoke.log 2>&1
    status=$?
    cat /tmp/kurt-smoke.log
    echo "SMOKE_EXIT=$status"
    test "$status" -eq 0

    grep -q '= kurt' /tmp/kurt-smoke.log
    grep -q '= 2' /tmp/kurt-smoke.log
    grep -Eq '^= ([A-HJ-Z][0-9]+|pass|resign)$' /tmp/kurt-smoke.log
  '';

  enterShell = ''
    echo "kurt dev shell: use 'build' or 'smoke'"
  '';
}
