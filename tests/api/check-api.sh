#!/usr/bin/env bash
#
# Assert that the built library reproduces its checked-in goldens.
#
# Usage: GHC="stack exec -- ghc" tests/api/check-api.sh
#
# The library must already be built.
#
# This checks the fork against its own record.  It does *not* compare it to
# upstream -- the two are deliberately different, because anything that cannot
# be ported faithfully is not exported at all.  That comparison is
# tests/api/check-subset.sh, which reads the upstream interface recorded in
# tests/api/upstream/ and requires every difference to be justified by name in
# unportable.txt.
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
#     fails until this backend has grown whatever upstream just added, or
#     recorded in unportable.txt why it will not.
#
# The goldens are GHC-version-sensitive: :browse output is pretty-printed by
# the compiler, and successive releases have changed how some types render.
# Regenerate when bumping the resolver, and expect that diff to be noise.

set -euo pipefail

cd "$(dirname "$0")/../.."

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

tests/api/dump-api.sh "$tmp" >/dev/null

status=0
for f in xmonad-api.golden xmonad-reexports.golden; do
    golden=tests/api/river/$f
    if diff -u "$golden" "$tmp/$f" > "$tmp/$f.diff"; then
        printf 'check-api: %-24s OK\n' "$f"
    else
        printf 'check-api: %-24s MISMATCH\n' "$f" >&2
        cat "$tmp/$f.diff" >&2
        status=1
    fi
done

if [ "$status" -ne 0 ]; then
    cat >&2 <<EOF

If this change was intended, regenerate in the same commit:

    GHC="\$GHC" tests/api/dump-api.sh tests/api/river
EOF
fi

exit "$status"
