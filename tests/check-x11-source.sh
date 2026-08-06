#!/usr/bin/env bash
#
# Assert that the package source is identical, with the river flag disabled, to
# the upstream base recorded in tests/upstream-base.sha.
#
# Usage: tests/check-x11-source.sh
#
# The river backend is built by *shadowing*: under -f river, cabal searches
# src-river before src, so src-river/XMonad/Core.hs wins over src/XMonad/Core.hs.
# Nothing under src/ is edited to accommodate river.  This script is what keeps
# that true.  If it passes, the X11 build compiles exactly the bytes it
# compiled at the recorded commit, and any river regression is confined to
# src-river/ by construction.
#
# Scope: every path the library compiles with the flag off.  src-river/ is
# deliberately not checked — it is the part that is allowed to move.

set -euo pipefail

cd "$(dirname "$0")/.."

sha_file=tests/upstream-base.sha
base=$(grep -vE '^\s*(#|$)' "$sha_file" | tr -d '[:space:]')

if [ -z "$base" ]; then
    echo "check-x11-source: no SHA found in $sha_file" >&2
    exit 1
fi

if ! git cat-file -e "$base^{commit}" 2>/dev/null; then
    echo "check-x11-source: $base is not a commit in this repository" >&2
    echo "  (recorded in $sha_file)" >&2
    exit 1
fi

# Paths the X11 build compiles.  Keep in sync with the library stanza's
# flag-off branch in xmonad.cabal.
paths=(src Main.hs)

status=0
for p in "${paths[@]}"; do
    if ! diff=$(git diff --stat "$base" -- "$p") || [ -n "$diff" ]; then
        if [ -n "$diff" ]; then
            echo "check-x11-source: $p differs from the recorded upstream base:" >&2
            git --no-pager diff "$base" -- "$p" >&2
            status=1
        fi
    fi
done

# Untracked files under those paths would be compiled too, and git diff cannot
# see them.
untracked=$(git ls-files --others --exclude-standard -- "${paths[@]}")
if [ -n "$untracked" ]; then
    echo "check-x11-source: untracked files would join the X11 build:" >&2
    printf '  %s\n' $untracked >&2
    status=1
fi

if [ "$status" -eq 0 ]; then
    echo "check-x11-source: OK — X11 sources match $base"
else
    cat >&2 <<EOF

The X11 build must not change as a side effect of river work.  If this change
was intended, record it by updating the SHA in $sha_file in the same commit.
EOF
fi

exit "$status"
