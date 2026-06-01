# Results

Current representative compiler benchmark:

- Native-built compiler: `_install/bin/ocamlopt.opt`
- LLVM-built compiler: `_llvm_self_stage_install/bin/ocamlopt.opt`
- Timed workload: both compilers compile representative compiler source files
  with the normal native backend.
- Latest rerun: `_compiler_binary_perf_current/summary_send_phys_20260526_155117.json`
- Aggregate: LLVM-built/native-built geomean `1.0897`, median `1.0910`.

Focused profile/assembly evidence:

- `typecore.ml` sample profile:
  `_compiler_binary_perf_current/profiles/current_20260526_155820`.
- `Env.find_same_without_locks` is much hotter at top of stack in the
  LLVM-built compiler profile: 54 samples vs 16 samples in the native-built
  compiler profile.
- `wrap_try` itself appears as 23 top-of-stack samples in the LLVM-built
  compiler profile.
- Current LLVM-built `ocamlopt.opt.real` has 2,979 direct calls to `_wrap_try`
  and about 4,956 `recover_rbp` symbols.
- Native assembly for `Env.find_same_without_locks` is 58 instructions in the
  focused comparison; LLVM assembly is 133 instructions.
- Native uses a compact trap stack sequence:
  `stp x26, handler, [sp, #-16]!; mov x26, sp; ...; ldr x26, [sp], #16`.
- LLVM calls `_wrap_try` in the protected hot path, stores a blockaddress in a
  global recovery slot, and carries extra recovery/runtime-register plumbing.

Reduced benchmark timing from the standalone runner:

- Command:
  `PAIRS=5 agent-state/llvm-fast-path-roots-integration/compiler_slowdown_benchmarks/run.sh`
- Log:
  `agent-state/llvm-fast-path-roots-integration/compiler_slowdown_benchmarks/run_main_20260526_163335.log`
- JSON:
  `agent-state/llvm-fast-path-roots-integration/compiler_slowdown_benchmarks/.build/summary.json`

```text
try_lookup_hit:     native=0.7406s llvm=0.8330s ratio=1.1247
try_lookup_miss:    native=0.5945s llvm=0.6081s ratio=1.0228
no_try_lookup_hit:  native=0.7433s llvm=0.7122s ratio=0.9581
string_tree_find:   native=0.8899s llvm=0.9494s ratio=1.0670
string_compare_loop:native=0.2460s llvm=0.3044s ratio=1.2374
string_map_find:    native=0.8832s llvm=0.9989s ratio=1.1310
```

Earlier exploratory timing of the same shape in the larger slow-case harness
was consistent:

```text
try_lookup_hit:  native=0.7117s llvm=0.7760s ratio=1.0903
try_lookup_miss: native=0.5636s llvm=0.6007s ratio=1.0658
```

The hit-path result is important. It shows the slowdown does not require taking
the exception. Installing the local handler and entering the protected call is
already enough to reproduce a compiler-like slowdown.

The no-handler control is also important. It uses the same hit-only lookup
helper call, but without a local handler in the loop LLVM is not slower. This
isolates the representative slowdown to the local handler around an OCaml call
that may raise.

The string cases show a second independent contributor. They have no local
handler and no `_wrap_try`, but LLVM is still slower. This matches the
profile/assembly evidence around `Ident.find_same`, where LLVM calls
`caml_string_compare` through a generated C-call wrapper while native emits the
direct runtime stack switch around `_caml_string_compare`.

Assembly shape from the standalone files:

```text
try_lookup_hit:    native .s 801 lines, llvm .s 1656 lines, 3 _wrap_try refs, 9 recover_rbp refs
try_lookup_miss:   native .s 810 lines, llvm .s 1658 lines, 3 _wrap_try refs, 9 recover_rbp refs
no_try_lookup_hit: native .s 771 lines, llvm .s 1407 lines, 0 _wrap_try refs, 0 recover_rbp refs
string_tree_find:  native .s 1146 lines, llvm .s 1914 lines, 0 _wrap_try refs,
                   3 c_call_wrapper.caml_string_compare refs
string_compare_loop:
                   native .s 786 lines, llvm .s 1417 lines, 0 _wrap_try refs,
                   3 c_call_wrapper.caml_string_compare refs
string_map_find:   native .s 4947 lines, llvm .s 7713 lines, 0 _wrap_try refs,
                   14 c_call_wrapper.caml_string_compare refs
```

Rejected candidates from `run_candidates_20260526_163050.log`:

```text
int_tree_find:      ratio=0.4957
poly_compare_tree:  ratio=0.9515
string_hash_table:  ratio=1.0053
apply2_loop:        ratio=1.0013
```

These are not good slowdown targets in the current setup. They are useful only
as negative evidence: this is not explained by tree recursion in general,
generic compare in general, string hashing in this small shape, or ordinary
apply2 overhead.

Control cases from the larger slow-case harness:

```text
try_no_raise_inside: native=0.1484s llvm=0.0762s ratio=0.5116
try_rare_raise:      native=0.0625s llvm=0.0622s ratio=0.9934
```

So the issue is not simply "LLVM is bad at try". The representative bad shapes
we have now are:

- local handler around an OCaml call that may raise;
- string comparison through generated C-call wrappers in lookup-heavy code.

Likely fix direction:

Implement a lower-overhead AArch64 trap lowering for this local-handler shape.
The target is native-like trap setup on the hot path while still making the
exceptional edge visible enough for LLVM correctness. The generated code should
avoid executing a `returns_twice` `_wrap_try` call every time through the hot
path.

Also investigate a lower-overhead contract for common runtime C calls such as
`caml_string_compare`, so LLVM does not need an out-of-line wrapper where native
can emit the stack switch inline at the call site.

The current LLVM lowering exists because LLVM needs to understand that the
handler is reachable and because calls inside an active trap need correct
exceptional control flow. A proper fix should preserve those contracts while
moving the hot path toward native's direct trap-block setup.
