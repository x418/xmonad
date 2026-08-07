#!/usr/bin/env bash
#
# Assert that the built library reproduces one backend's checked-in goldens.
#
# Usage: GHC="stack exec -- ghc" tests/api/check-api.sh {x11|river}
#
# The library must already be built with the matching flag:
#
#     stack build                     && ... check-api.sh x11
#     stack build --flag xmonad:river && ... check-api.sh river
#
# This checks each backend against its own record.  It does *not* compare the
# two against each other -- they are deliberately different, because anything
# river cannot port faithfully is not exported at all.  The cross-backend
# assertion is tests/api/check-subset.sh, which requires every difference to be
# justified by name in unportable.txt or river-only.txt.
#
# A mismatch here is one of two things, and telling them apart is the
# reviewer's job rather than this script's:
#
#   * an unintended API change -- a name lost or gained without meaning to;
#
#   * an intended one, usually a rebase onto xmonad HEAD that added an export.
#     Regenerate with dump-api.sh in the same commit, so the golden diff sits
#     next to the change that caused it.  For a fork whose purpose is tracking
#     upstream, that second case is the more valuable of the two: the golden
#     fails until the river backend has grown whatever upstream just added, or
#     recorded in unportable.txt why it will not.
#
# The goldens are GHC-version-sensitive: :browse output is pretty-printed by
# the compiler, and successive releases have changed how some types render.
# Regenerate when bumping the resolver, and expect that diff to be noise.

set -euo pipefail

cd "$(dirname "$0")/../.."

backend=${1:-}
case "$backend" in
    x11|river) ;;
    *) echo "usage: check-api.sh {x11|river}" >&2; exit 1 ;;
esac

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

tests/api/dump-api.sh "$tmp" >/dev/null

status=0
for f in xmonad-api.golden xmonad-reexports.golden; do
    golden=tests/api/$backend/$f
    if diff -u "$golden" "$tmp/$f" > "$tmp/$f.diff"; then
        printf 'check-api[%s]: %-24s OK\n' "$backend" "$f"
    else
        printf 'check-api[%s]: %-24s MISMATCH\n' "$backend" "$f" >&2
        cat "$tmp/$f.diff" >&2
        status=1
    fi
done

if [ "$status" -ne 0 ]; then
    cat >&2 <<EOF

If this change was intended, regenerate in the same commit:

    GHC="\$GHC" tests/api/dump-api.sh tests/api/$backend
EOF
fi

exit "$status"
