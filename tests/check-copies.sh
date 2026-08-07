#!/usr/bin/env bash
#
# Assert how each src-river/ module relates to its src/ counterpart.
#
# Usage: tests/check-copies.sh          verify
#        tests/check-copies.sh --regen  rewrite the patches from the current tree
#
# src-river/ is a complete second copy of the library, so most of its modules
# differ from src/ by design -- that is the backend.  But "differs" spans two
# very different situations, and conflating them is how a fork rots:
#
#   IDENTICAL   byte-for-byte.  Nothing else in the test suite can tell when a
#               verbatim copy stops being verbatim: the API goldens compare
#               signatures, so a fix to XMonad.StackSet's arithmetic on one
#               side only would leave both goldens green while the two builds
#               quietly disagreed about what focusUp does.
#
#   PATCHED     a small, tracked divergence, kept as a unified diff under
#               patches/.  Applying the patch to src/ must reproduce src-river/
#               exactly.  These are the files where most of the code is
#               backend-independent -- XMonad.Core is 84% shared, and holds
#               recompile, getDirectories and the whole compile path, which
#               upstream will keep fixing.  When it does, `patch` either
#               applies the fix or conflicts loudly, and either beats
#               discovering months later that only one copy was fixed.
#
#   REWRITTEN   genuinely different programs that agree on an interface.
#               XMonad.Operations is 899 lines of Xlib upstream and 360 here.
#               There is nothing to diff and nothing to check; the API goldens
#               and the subset assertion are what hold these honest.
#
# The manifests below are exact in both directions.  A file that moves between
# categories fails until the manifest says so -- including a file that becomes
# *more* similar, which usually means a river module has been simplified back
# toward upstream and the question is whether it should still be a copy at all.

set -euo pipefail

cd "$(dirname "$0")/.."

# Byte-identical copies.
#
# XMonad.StackSet is pure, total, and mentions neither backend -- the one
# module the port did not have to think about.  Any difference at all is a bug.
identical=(
    XMonad/StackSet.hs
)

# Tracked divergences, as patches/<path>.patch.
patched=(
    XMonad.hs
    XMonad/Core.hs
    XMonad/Layout.hs
)

# Everything else under src/ that also exists in src-river/ is a rewrite, and
# is deliberately unchecked here.

regen=false
[ "${1:-}" = "--regen" ] && regen=true

if $regen; then
    for f in "${patched[@]}"; do
        mkdir -p "patches/$(dirname "$f")"
        diff -u "src/$f" "src-river/$f" > "patches/$f.patch" || true
        printf 'regenerated  patches/%s.patch\n' "$f"
    done
    exit 0
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

status=0

# --- identical ---------------------------------------------------------------

printf '%s\n' "${identical[@]}" | sort > "$tmp/claimed-identical"

: > "$tmp/actual-identical"
while IFS= read -r f; do
    f=${f#./}
    [ -f "src-river/$f" ] || continue
    if diff -q "src/$f" "src-river/$f" >/dev/null 2>&1; then
        echo "$f" >> "$tmp/actual-identical"
    fi
done < <(cd src && find . -name '*.hs' | sort)
sort -o "$tmp/actual-identical" "$tmp/actual-identical"

drifted=$(comm -13 "$tmp/actual-identical" "$tmp/claimed-identical")
if [ -n "$drifted" ]; then
    echo "check-copies: listed as identical, but is not:" >&2
    printf '  %s\n' $drifted >&2
    echo "  Port the change to the other copy, or move the entry to \$patched." >&2
    status=1
fi

converged=$(comm -23 "$tmp/actual-identical" "$tmp/claimed-identical")
if [ -n "$converged" ]; then
    echo "check-copies: identical to src/, but not listed as identical:" >&2
    printf '  %s\n' $converged >&2
    echo "  If intended, add it to \$identical in $0." >&2
    status=1
fi

[ -z "$drifted$converged" ] &&
    printf 'check-copies: %-12s OK  %s\n' identical "${identical[*]}"

# --- patched -----------------------------------------------------------------

for f in "${patched[@]}"; do
    p="patches/$f.patch"
    if [ ! -f "$p" ]; then
        echo "check-copies: missing $p (run tests/check-copies.sh --regen)" >&2
        status=1
        continue
    fi
    if ! patch -s --no-backup-if-mismatch -o "$tmp/out" "src/$f" "$p" 2>"$tmp/err"; then
        echo "check-copies: $p does not apply to src/$f:" >&2
        sed 's/^/    /' "$tmp/err" >&2
        status=1
        continue
    fi
    if diff -q "$tmp/out" "src-river/$f" >/dev/null; then
        printf 'check-copies: %-12s OK  %-22s %4d line patch\n' \
               patched "$f" "$(wc -l < "$p")"
    else
        echo "check-copies: src/$f + $p does not reproduce src-river/$f:" >&2
        diff -u "$tmp/out" "src-river/$f" | head -40 >&2
        echo "  If src-river/$f changed on purpose, run tests/check-copies.sh --regen." >&2
        status=1
    fi
done

exit "$status"
