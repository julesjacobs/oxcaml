# Testing Improvements

- [x] Add an AMD64 exception recovery regression test covering stack growth,
  exception raise/catch, allocation after recovery, and backtrace state.
- [x] Add a GC safepoint SIMD liveness test keeping `float32`, `vec128`, and
  optionally `vec256` values live across allocation or poll safepoints.
- [x] Add a reduced mixed C ABI / unboxed primitive argument stress test for
  `float32`, `int64x2`, `vec128`, stack arguments, and alloc/noalloc externals.
- [x] Add shell-level negative tests for `tools/run-llvm-stage5-ocamltest.sh`
  covering bad list entries, `..` entries, absolute entries, and unique temp
  path behavior.
- [x] Add LLVM IR or assembly shape checks for fragile paths such as
  `no-realign-stack`, GC helper selection, PIC-safe recovery, and vector
  calling-convention lowering.
- [x] Run validation from a clean clone or clean worktree to catch dependence
  on stale `_llvm_*` or `_build` artifacts.
- [x] Repeat the self-stage validation after deleting stage directories to
  check temp-file, staged-install, and stale-state repeatability.
