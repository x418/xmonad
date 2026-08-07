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
#   2. River's eight modules add *nothing* to xmonad's API.  Everything the
#      river backend offers that xmonad does not lives in XMonad.River, which
#      this check does not look at -- so a config importing XMonad sees exactly
#      the names the X11 build offers, minus the unportable ones, and never
#      more.  A river-specific name appearing in XMonad.Core is a failure whose
#      fix is to move it to XMonad.River.  The comparison is against the union
#      of X11's own surface and its Graphics.X11 re-exports, so a name that
#      merely moves between those two files is not flagged.
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

if [ -s "$tmp/added" ]; then
    echo "check-subset: these have no X11 counterpart and belong in XMonad.River:" >&2
    sed 's/^/  /' "$tmp/added" >&2
    status=1
else
    printf 'check-subset: %-34s OK (0)\n' "names added by river"
fi

if [ -s "$river_reexports" ]; then
    echo "check-subset: river re-exports an X11-shaped compat surface:" >&2
    head -20 "$river_reexports" >&2
    echo "  river must not imitate Graphics.X11; see tests/api/unportable.txt" >&2
    status=1
else
    printf 'check-subset: %-34s OK (0)\n' "river X11 re-exports"
fi

exit "$status"
