- [ ] Revisit CI setup

- [ ] Handle mgsloan's patches - this branch was accidentally based on 4 patches
  to xmonad. Either merge something similar upstream or undo the effects of those.

- [ ] Assert that some modules are identical with upstream xmonad.

- [ ] **Nothing tests the loop.** The suites cover `XMonad.StackSet` and the
  wire codec. The concurrency invariants — no torn reads, ops delivered exactly
  once, liveness filtering complete — are precisely what property tests cannot
  reach. `tests/headless-river.sh` and `tests/headless-prompt.sh` are the only
  harnesses that run against a real compositor, and they would need extending.

- [ ] Rename closeAllPrompts. Utility to generically interrupt the worker?
