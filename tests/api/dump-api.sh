#!/usr/bin/env bash
#
# Emit the normalized public API surface of an xmonad library.
#
# Usage: tests/api/dump-api.sh OUTDIR [CHECKOUT]
#
#   tests/api/dump-api.sh tests/api/river                 # this fork
#   tests/api/dump-api.sh tests/api/upstream ../xmonad    # upstream xmonad
#
#   OUTDIR receives:
#     xmonad-api.golden        xmonad's own 200-odd names, across all 8 modules
#     xmonad-reexports.golden  the names XMonad re-exports from an
#                              X11-shaped compatibility surface
#     source.sha               which commit was dumped -- only when CHECKOUT
#                              is given, since otherwise it is this working
#                              tree and git already knows
#
# CHECKOUT is what makes one script do both jobs, and there is only one job:
# dumping whatever xmonad is registered.  Point it at another checkout and it
# builds that one and reads its package database instead of this one's.  That
# is the whole difference between recording this fork and recording upstream.
#
# Environment:
#   GHC        how to invoke ghc.  Default "ghc".  Under stack, use
#              GHC="stack exec -- ghc"; under cabal, "cabal exec -- ghc".
#              Ignored when CHECKOUT is given -- that build's own stack is
#              used, since the point is to read its package database.
#   GHC_FLAGS  extra flags, e.g. -package-db for a non-default build dir.
#
# The point of this script: this fork's API must remain a strict subset of
# upstream's, so that xmonad-contrib and an unmodified xmonad.hs compile
# against it.  Dumping both is what lets tests/api/check-subset.sh compare
# them without building anything itself.  Upstream is a recording rather than a
# build because this package no longer carries upstream's sources.
#
# Mechanism: `ghc -e ':browse'` rather than `ghc --show-iface`.  --show-iface
# does emit an export list, but its declaration section is Core with unfoldings
# attached, and it prints neither record field names nor class bodies in a form
# worth diffing.  :browse prints exactly the source-level surface: signatures,
# data declarations with their fields, and class declarations with their
# methods.  Field names are what catch the XConf hazard described in the design.
#
# Normalization: names print fully qualified by their *defining* module, so the
# X11 re-exports appear under Graphics.X11.* in one build and under
# XMonad.Backend.Compat.* in the other.  Module qualification is therefore
# stripped, and whitespace is collapsed afterwards -- GHC's line wrapping
# depends on name lengths, so a longer module name reflows a signature that is
# otherwise identical.
#
# What is deliberately *not* normalized away: arity, argument order, type
# variables, constraints, class methods, and constructor and field names.  A
# signature that gains `MonadIO m =>` is an API break and must show up as one.
#
# What this test cannot see: that river's Display is a Connection.  The
# comparison is name- and shape-based, so semantic divergence is invisible to
# it by construction.  That is what the tier-3 table in README.river.md records.

set -euo pipefail

outdir=${1:?usage: dump-api.sh OUTDIR [CHECKOUT]}
checkout=${2:-}

mkdir -p "$outdir"
outdir=$(cd "$outdir" && pwd -P)

GHC=${GHC:-ghc}
GHC_FLAGS=${GHC_FLAGS:-}

if [ -n "$checkout" ]; then
    [ -f "$checkout/xmonad.cabal" ] || {
        echo "dump-api: $checkout is not an xmonad checkout" >&2; exit 1; }
    checkout=$(cd "$checkout" && pwd -P)
    sha=$(git -C "$checkout" rev-parse HEAD)

    # Not refused, only reported.  A fork's tool is going to be pointed at
    # trees carrying local patches; what matters is that "recorded from a tree
    # nobody else has" is visible when the golden later disagrees with someone.
    if ! git -C "$checkout" diff --quiet HEAD; then
        echo "dump-api: WARNING: $checkout has uncommitted changes;" >&2
        echo "          the recording will include them" >&2
    fi

    echo "dump-api: building $checkout at $sha" >&2
    ( cd "$checkout" && stack build ) >/dev/null
    cd "$checkout"
    GHC="stack exec -- ghc"

    printf '%s\n' \
      "# The commit whose interface is recorded beside this file." \
      "#" \
      "# Written by tests/api/dump-api.sh when given a checkout to read." \
      "# Nothing forces a re-record when that checkout moves on, so this is how" \
      "# a reviewer tells how old the comparison in check-subset.sh has become." \
      "$sha" > "$outdir/source.sha"
fi

modules=(
    XMonad
    XMonad.Config
    XMonad.Core
    XMonad.Layout
    XMonad.Main
    XMonad.ManageHook
    XMonad.Operations
    XMonad.StackSet
)

# Modules whose names are the backend's X11-shaped compatibility surface.  Only
# one of these ever exists in a given build; listing both keeps the script
# itself backend-agnostic.
compat_prefixes='Graphics\.X11|XMonad\.Backend\.Compat'

raw=$(mktemp)
trap 'rm -f "$raw"' EXIT

for m in "${modules[@]}"; do
    # shellcheck disable=SC2086  # GHC_FLAGS is intentionally word-split
    $GHC $GHC_FLAGS -ignore-dot-ghci -e ":browse $m" \
        | sed "s|^|$m\t|" \
        >> "$raw"
done

perl -e '
    my ($compat, $api_out, $reexport_out) = @ARGV[0..2];

    my (@api, @reexports);
    my ($module, $entry);

    # Flush one logical entry: classify it by the module that *defines* it,
    # then strip qualification and collapse whitespace.
    my $flush = sub {
        return unless defined $entry;
        # The defining module is the qualifier of the first qualified name in
        # the entry.  For `Graphics.X11.Types.xK_a :: KeySym` that is the name
        # itself; for a class whose superclass constraint comes first it may be
        # base, which correctly lands the entry in the api file.
        my $is_reexport = 0;
        if ($entry =~ /(?:[A-Za-z0-9_.-]+:)?((?:[A-Z][A-Za-z0-9_'"'"']*\.)+)/) {
            my $mod = $1;
            $mod =~ s/\.$//;
            $is_reexport = 1 if $mod =~ /^(?:$compat)(?:\.|$)/;
        }

        my $text = $entry;
        # base-4.17.2.0:Data.Semigroup.Internal.Endo -> Endo
        # Graphics.X11.Types.Window                  -> Window
        # (GHC.Bits..|.)                             -> (.|.)
        $text =~ s/(?:[A-Za-z0-9_.-]+:)?(?:[A-Z][A-Za-z0-9_'"'"']*\.)+//g;
        # Unboxing decisions are representation, not API: -funbox-strict-fields
        # attaches {-# UNPACK #-} and a (N:CUInt[0]) newtype-coercion note to
        # fields whose type happens to be a single-constructor newtype.  Two
        # backends may unbox differently while exporting the same field.
        $text =~ s/\{-# UNPACK #-\}//g;
        $text =~ s/\(N:[^)]*\)//g;
        # Strictness annotations are printed only when the field could not be
        # unboxed, which depends on how the field is represented rather than on
        # what the source says.  Both backends write a bang on KeyMask fields;
        # GHC renders the X11 one because that KeyMask is a newtype over CUInt,
        # and drops it for river because Word32 unboxes.  Same source, same
        # semantics, different rendering -- so it has to go, or every strict
        # field becomes a false difference.
        $text =~ s/!\s*//g;
        $text =~ s/\s+/ /g;
        $text =~ s/^ | $//g;

        # The declared name, so that the subset check can compare names without
        # being confused by a signature that differs for good reason.
        my $name;
        if    ($text =~ /^(?:type|data|newtype) (?:family |instance |role )?(\S+)/) { $name = $1 }
        elsif ($text =~ /^class (?:.*=> )?(\S+)/)                                   { $name = $1 }
        elsif ($text =~ /^(\(.*?\)|[^\s:]+) ::/)                                    { $name = $1 }
        else                                                                        { $name = $text }

        push @{ $is_reexport ? \@reexports : \@api }, "$module | $name | $text\n";
        $entry = undef;
    };

    while (my $line = <STDIN>) {
        chomp $line;
        my ($mod, $rest) = split /\t/, $line, 2;
        $rest = "" unless defined $rest;
        next if $rest =~ /^\s*$/;

        if ($rest =~ /^\s/) {
            # Continuation of the entry in progress.
            $entry .= " $rest" if defined $entry;
        } else {
            $flush->();
            $module = $mod;
            $entry  = $rest;
        }
    }
    $flush->();

    for my $spec ([$api_out, \@api], [$reexport_out, \@reexports]) {
        my ($path, $lines) = @$spec;
        open my $fh, ">", $path or die "$path: $!";
        print $fh $_ for sort @$lines;
        close $fh;
    }
' "$compat_prefixes" "$outdir/xmonad-api.golden" "$outdir/xmonad-reexports.golden" < "$raw"

printf 'api:       %6d entries -> %s\n' \
    "$(wc -l < "$outdir/xmonad-api.golden")" "$outdir/xmonad-api.golden"
printf 'reexports: %6d entries -> %s\n' \
    "$(wc -l < "$outdir/xmonad-reexports.golden")" "$outdir/xmonad-reexports.golden"
