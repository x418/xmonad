#!/usr/bin/env bash
#
# Every consistency check, in an order that is actually valid.
#
# Usage: tests/check-all.sh
#
# The ordering is the point.  `ghc -e :browse` reads whichever build of the
# library is currently registered, so dumping or checking one backend's API
# while the other is installed silently compares the wrong thing -- and the
# failure looks like a real API regression, complete with a plausible diff.
# That has cost time more than once.  Each API check therefore immediately
# follows the build that makes it meaningful, and nothing is interleaved.
#
# This is what CI runs; keep .github/workflows/river.yml in step.

set -euo pipefail

cd "$(dirname "$0")/.."

GHC=${GHC:-stack exec -- ghc}
export GHC

step() { printf '%-22s ' "$1"; }
ok()   { echo OK; }

step "x11 source frozen"; ./tests/check-x11-source.sh >/dev/null; ok

step "x11 build";         stack build --flag xmonad:pedantic >/dev/null 2>&1; ok
step "x11 api golden";    ./tests/api/check-api.sh x11 >/dev/null; ok

step "river build";       stack build --flag xmonad:river --flag xmonad:pedantic >/dev/null 2>&1; ok
step "river api golden";  ./tests/api/check-api.sh river >/dev/null; ok
step "river tests";       stack test --flag xmonad:river >/dev/null 2>&1; ok

# These read the checked-in goldens rather than the built library, so they do
# not care which backend is registered.
step "subset assertion";  ./tests/api/check-subset.sh >/dev/null; ok
step "copy manifest";     ./tests/check-copies.sh >/dev/null; ok

step "keysyms current"
./codegen/gen-keysyms.sh >/dev/null
git diff --quiet -- src-river/XMonad/River/Keysym.hs src-river/XMonad/River/Keysym/Table.hs
ok

echo
echo "all checks passed"
