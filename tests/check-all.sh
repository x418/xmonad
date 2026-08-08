#!/usr/bin/env bash
#
# Every consistency check, in an order that is actually valid.
#
# Usage: tests/check-all.sh
#
# The ordering still matters.  `ghc -e :browse` reads whichever build of the
# library is currently registered, so the API check has to follow the build
# that makes it meaningful.  This used to matter far more, when two backends
# were built from one repo and checking one while the other was installed
# silently compared the wrong thing -- a failure that looked like a real API
# regression, complete with a plausible diff.
#
# This is what CI runs; keep .github/workflows/river.yml in step.

set -euo pipefail

cd "$(dirname "$0")/.."

GHC=${GHC:-stack exec -- ghc}
export GHC

step() { printf '%-22s ' "$1"; }
ok()   { echo OK; }

step "build";             stack build --flag xmonad:pedantic >/dev/null 2>&1; ok
step "api golden";        ./tests/api/check-api.sh >/dev/null; ok
step "tests";             stack test >/dev/null 2>&1; ok

# Reads the checked-in goldens rather than the built library: this fork's, and
# upstream's as recorded by dump-api.sh from an upstream checkout.  Nothing is
# built here, which is why losing the second source tree did not cost the
# subset assertion.
step "subset assertion";  ./tests/api/check-subset.sh >/dev/null; ok

step "keysyms current"
./codegen/gen-keysyms.sh >/dev/null
git diff --quiet -- src/XMonad/River/Keysym.hs src/XMonad/River/Keysym/Table.hs
ok

echo
echo "all checks passed"
