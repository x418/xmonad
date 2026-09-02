# The shell stack builds in on NixOS (stack.yaml: nix.shell-file).
#
# Stack can generate this itself from nix.packages, and this is that
# expression (src/Stack/Nix.hs) with one difference: it says
# `stdenv.hostPlatform.isLinux` where stack's says `stdenv.isLinux`, which
# nixpkgs has deprecated, so every `stack exec` printed a deprecation
# warning.  nixpkgs' buildStackProject would be the natural replacement, but
# on current nixpkgs it puts a list into `env` and fails to evaluate.
#
# `ghc` is the compiler stack's resolver asks for; stack supplies both.
{ ghc, ghcVersion ? null }:
with (import <nixpkgs> { });
let
  # What the cabal file's pkgconfig-depends and c-sources need; what
  # util/gen-keysyms.sh reads libxkbcommon's keysym header through; and curl,
  # which util/generate-protocol.hs fetches the protocol XML with -- the
  # shell is pure, so the system's curl is not on its PATH.  No X11: nothing
  # here links or builds it.  The rest is what stack always adds.
  inputs = [ zlib pkg-config libxkbcommon curl ghc git gcc gmp cacert ];
in
runCommand "xmonad-river-stack-shell" {
  # glibcLocales, so GHC can set a locale.
  buildInputs = lib.optional stdenv.hostPlatform.isLinux glibcLocales ++ inputs;
  STACK_PLATFORM_VARIANT = "nix";
  STACK_IN_NIX_SHELL = 1;
  # Template Haskell still needs the libraries on the loader path.
  LD_LIBRARY_PATH = lib.makeLibraryPath inputs;
  STACK_IN_NIX_EXTRA_ARGS = lib.concatMap (pkg: [
    "--extra-lib-dirs=${lib.getLib pkg}/lib"
    "--extra-include-dirs=${lib.getDev pkg}/include"
  ]) inputs;
  # Unicode output from base works whether or not the system has en_US.UTF-8.
  LANG = "C.UTF-8";
} ""
