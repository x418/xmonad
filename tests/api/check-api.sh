#!/usr/bin/env bash
#
# Assert that the built library reproduces the checked-in API golden files.
#
# Usage: GHC="stack exec -- ghc" tests/api/check-api.sh
#
# Run this once per flag setting.  Both backends must produce the same two
# files, because the point of the fork is that xmonad-contrib and an unmodified
# xmonad.hs compile against either.
#
# A mismatch is one of two things, and the distinction is the reviewer's job,
# not this script's:
#
#   * an API break — the river backend lost a name, or gained one X11 does not
#     have, and contrib will not compile against it;
#
#   * an intentional change — usually a rebase onto xmonad HEAD that added an
#     export.  Regenerate with tests/api/dump-api.sh in the same commit, so the
#     golden diff sits next to the change that caused it.
#
# The second case is the more valuable of the two guarantees for a fork whose
# whole purpose is tracking upstream: the golden file fails until the river
# backend grows the name upstream just added.
#
# Note the goldens are GHC-version-sensitive: :browse output is pretty-printed
# by the compiler, and successive GHC releases have changed how they render
# some types.  Regenerate when bumping the resolver, and expect that diff to be
# noise rather than signal.

set -euo pipefail

cd "$(dirname "$0")/../.."

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

tests/api/dump-api.sh "$tmp" >/dev/null

status=0
for f in xmonad-api.golden xmonad-reexports.golden; do
    if diff -u "tests/api/$f" "$tmp/$f" > "$tmp/$f.diff"; then
        printf 'check-api: %-24s OK\n' "$f"
    else
        printf 'check-api: %-24s MISMATCH\n' "$f" >&2
        cat "$tmp/$f.diff" >&2
        status=1
    fi
done

if [ "$status" -ne 0 ]; then
    cat >&2 <<'EOF'

If this change was intended, regenerate in the same commit:

    GHC="stack exec -- ghc" tests/api/dump-api.sh tests/api
EOF
fi

exit "$status"
