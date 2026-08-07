#!/usr/bin/env bash
#
# Assert that river's API is the intended subset of the X11 one.
#
# Usage: tests/api/check-subset.sh
#
# Reads the two backends' checked-in goldens; builds nothing.  Regenerate them
# first with tests/api/dump-api.sh if either backend has changed:
#
#     stack build                     && GHC="stack exec -- ghc" tests/api/dump-api.sh tests/api/x11
#     stack build --flag xmonad:river && GHC="stack exec -- ghc" tests/api/dump-api.sh tests/api/river
#
# Three assertions, each of which has caught something worth catching:
#
#   1. Every name the X11 build exports and river does not is justified in
#      unportable.txt -- and every name justified there is genuinely absent.
#      The second half matters as much as the first: a stale entry means the
#      file is describing a backend that no longer exists.
#
#   2. Every name river exports that X11 has no counterpart for is justified in
#      river-only.txt, in both directions likewise.  The comparison is against
#      the union of X11's own surface and its Graphics.X11 re-exports, so a
#      name that merely moves between those two files is not flagged.
#
#   3. River re-exports nothing from a Graphics.X11-shaped compat surface.
#      This is the positive form of the rule the whole backend is built on:
#      river does not imitate an X11 that is not there.

set -euo pipefail

cd "$(dirname "$0")/../.."

api=tests/api
x11_api=$api/x11/xmonad-api.golden
x11_reexports=$api/x11/xmonad-reexports.golden
river_api=$api/river/xmonad-api.golden
river_reexports=$api/river/xmonad-reexports.golden

for f in "$x11_api" "$x11_reexports" "$river_api" "$river_reexports"; do
    [ -f "$f" ] || { echo "check-subset: missing $f" >&2; exit 1; }
done

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Column 2 of a golden line is the declared name.
names() { cut -d'|' -f2 "$@" | sed 's/^ *//; s/ *$//' | sort -u; }

# A justification file is `name  reason`, with # comments and continuations.
listed() { grep -vE '^\s*(#|$)' "$1" | awk '{print $1}' | sort -u; }

names "$x11_api"                       > "$tmp/x11-api"
names "$x11_api" "$x11_reexports"      > "$tmp/x11-all"
names "$river_api"                     > "$tmp/river-api"

comm -23 "$tmp/x11-api"  "$tmp/river-api" > "$tmp/missing"
comm -13 "$tmp/x11-all"  "$tmp/river-api" > "$tmp/added"

listed "$api/unportable.txt" > "$tmp/unportable"
listed "$api/river-only.txt" > "$tmp/river-only"

status=0

report() {
    local label=$1 actual=$2 claimed=$3 file=$4 verb=$5
    local undocumented documented_but_absent
    undocumented=$(comm -23 "$actual" "$claimed")
    documented_but_absent=$(comm -13 "$actual" "$claimed")

    if [ -n "$undocumented" ]; then
        echo "check-subset: $label not justified in $file:" >&2
        printf '  %s\n' $undocumented >&2
        status=1
    fi
    if [ -n "$documented_but_absent" ]; then
        echo "check-subset: $file lists names that are $verb:" >&2
        printf '  %s\n' $documented_but_absent >&2
        status=1
    fi
    if [ -z "$undocumented$documented_but_absent" ]; then
        printf 'check-subset: %-34s OK (%d)\n' "$label" "$(wc -l < "$actual")"
    fi
}

report "names dropped by river" "$tmp/missing" "$tmp/unportable" \
       "$api/unportable.txt" "not actually dropped"
report "names added by river"   "$tmp/added"   "$tmp/river-only" \
       "$api/river-only.txt" "not actually exported"

if [ -s "$river_reexports" ]; then
    echo "check-subset: river re-exports an X11-shaped compat surface:" >&2
    head -20 "$river_reexports" >&2
    echo "  river must not imitate Graphics.X11; see tests/api/unportable.txt" >&2
    status=1
else
    printf 'check-subset: %-34s OK (0)\n' "river X11 re-exports"
fi

exit "$status"
