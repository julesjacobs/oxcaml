# Progress

- 2026-06-30 matched no-debug diagnostic for the current best BOLT artifact:
  - Checked whether the remaining compiler-throughput gap is mostly debug
    frametable work by rerunning the five-module compiler benchmark without
    `-g`. This was a diagnostic only; dropping `-g` is not a candidate
    optimization for the goal.
  - An initial reduction accidentally ran the BOLT compiler against the
    native-built `OCAMLLIB`/build tree and produced a misleading segfault. The
    corrected run matches the validated setup: native compiler with
    `_native_current_build/main` + `_native_current_install/lib/ocaml`, and the
    BOLT compiler with `_llvm_constfilter_build/main` +
    `_llvm_constfilter_install/lib/ocaml`.
  - Corrected no-`-g` result, `samples=3`, `inner_repetitions=3`: native median
    `26.917524s`, best BOLT median `26.129731s`, ratio `0.970733`, improvement
    `+2.93%`
    (`native-current-vs-best-bolt-no-debug-matched-diagnostic-samples3-inner3.json`).
  - This is below the robust `-g` best (`+3.77%`) and nowhere near the required
    LLVM-path-only `+6%`. Debug-info generation or debug-only frametable
    descriptors are therefore not the missing win by themselves; the remaining
    gap still points at broader AMD64 LLVM root/spill and allocator/code-shape
    issues, especially the weak `typing/ctype.ml` module result.

- 2026-06-30 rejected corrected-gate `-split-spill-mode=default` as a
  compiler-throughput path:
  - Retested the global split-spill diagnostic under the corrected boot gate
    after discovering the old `_install` stage0 was stale. The corrected gate
    uses `_native_current_install` as stage0 plus the canonical
    `tools/llvm-rs4gc-llc-wrapper.sh` with local LLVM tools on `PATH`.
  - The serialized boot build passed and the smoke test printed `55`
    (`1682` wrapper lines, `841` fresh IR, then `4` smoke wrapper lines / `2`
    fresh IR). This overturns the old correctness rejection: the earlier
    `.ocamlcommon` failure was a bad-stage0 validation artifact, not proof
    that `-split-spill-mode=default` is boot-unsafe.
  - Performance still rejects it for the current goal. On the same quick
    three-sample, one-inner five-module compiler screen, native median was
    `9.029214s`, the normal LLVM boot compiler was previously `11.617564s`,
    and the split-default LLVM boot compiler was `11.563912s`. That is only a
    tiny comparable-boot improvement over normal LLVM and still far behind
    native, not a route to the required LLVM-path-only `+6%`.
  - Removed the temporary split-default wrapper. The result file is the
    untracked local artifact
    `native-current-vs-splitdefault-boot-samples3-inner1.json`.

- 2026-06-30 corrected boot gate and re-screened post-statepoint fold block:
  - Rechecked the earlier post-statepoint spill-fusing diagnostic after finding
    the previous boot rejection used the stale `_install` stage0. The normal
    canonical LLVM wrapper also failed with `_install` as stage0, so that was
    not a valid candidate-specific correctness gate.
  - Established a clean gate using `_native_current_install` as stage0 and the
    canonical `tools/llvm-rs4gc-llc-wrapper.sh` with local LLVM tools on
    `PATH`. A serialized boot build passed and its smoke test printed `55`
    (`1682` wrapper lines, `841` fresh IR, then `4` smoke wrapper lines / `2`
    fresh IR).
  - Retried the temporary hidden
    `-disable-post-statepoint-spill-fusing` diagnostic under that corrected
    gate. It also passed the serialized boot build and smoke with the same
    wrapper counts. This means the diagnostic is boot-safe at this level; the
    old failure was a stale-stage0/build-gate issue, not proof that this
    candidate miscompiled the compiler.
  - Performance still rejects it as a compiler-throughput path. A quick
    three-sample, one-inner five-module screen of the post-statepoint boot
    compiler against native was slow, but so was the normal LLVM boot compiler:
    normal boot median `11.617564s`, post-statepoint boot median `11.594173s`
    versus native around `9.0s`. The diagnostic is only a tiny boot-compiler
    improvement over comparable normal LLVM boot, not a route to `+6%`.
    Results are in untracked local artifacts
    `native-current-vs-canonical-boot-samples3-inner1.json` and
    `native-current-vs-poststatepoint-boot-samples3-inner1.json`.
  - Removed the temporary source/wrapper again and rebuilt `llc` from restored
    sources. The useful takeaways are: use `_native_current_install` for future
    boot gates, and keep looking for a root/spill improvement that moves the
    compiler workload, not just the small loop microbenchmark.

- 2026-06-30 rejected post-statepoint spill-fusing block as a compiler candidate:
  - Reconfirmed the scope correction: generic driver/compiler optimization
    changes such as `-O4` cannot count toward the +6% target because the
    native-built compiler can receive the same treatment. Candidate wins must
    be LLVM-path-only: backend/codegen, LLVM-built-only pass configuration, or
    LLVM-built binary post-link work such as BOLT.
  - Tested a temporary hidden `llc` switch that skipped folding spill reloads
    into instructions after a same-block statepoint in `oxcaml`/`ocaml` GC
    functions. Focused `typing/ctype.ml` RS4GC stats showed the switch hit the
    intended shape without changing root metadata: OXSR stayed at `1629`
    appended spill slots, fixup stayed at `407` spill slots / `798` spilled
    registers, frame size stayed `27936` bytes, while regalloc skipped `1574`
    post-statepoint folds and inserted `47` more reloads.
  - The focused dynamic loop microbenchmark confirmed this is a real LLVM-only
    control point for the old folded-memory-ALU slowdown: with the normal
    wrapper the GC loop cases were about `1.53x` to `1.56x` slower than native;
    with the temporary switch they were about `1.07x` slower on the same
    reduced five-sample setup.
  - Full compiler boot validation rejected the switch. A fresh boot build
    through `tools/build-llvm-boot-with-installed.sh`, using only the targeted
    wrapper flag, failed after roughly `402` wrapper invocations with the same
    hard correctness class as prior broad allocator experiments:
    `.ocamlcommon` reported `Fatal error: allocation failure during minor GC`
    and the byte side took `SEGV`. I stopped the remaining build jobs after
    that failure and reverted the temporary source/wrapper, then rebuilt `llc`
    from restored sources.
  - Conclusion: harmful post-statepoint scalar reload folding is still an
    important measured slowdown class, but simply blocking all same-block
    post-statepoint folding is too broad or violates an allocator/root
    invariant. The next valid route is to reduce the `.ocamlcommon` failure
    and identify the exact unsafe subcase, or design a narrower transform that
    preserves the microbenchmark win and passes boot/self-stage validation.

- 2026-06-30 rejected targeted OxCaml statepoint partition spill mode:
  - Followed up on the global `-split-spill-mode=default` signal with a
    temporary hidden diagnostic that kept the normal greedy `speed` mode
    everywhere except OxCaml GC virtual registers used as statepoint varargs,
    where global/block splitting used partitioned complement mode. This was
    intended to target the bad statepoint root shape without changing generic
    LLVM/X86 allocation.
  - Focused `typing/ctype.ml` screen reproduced the same promising counter
    movement as the global mode: total appended spill slots dropped from
    `1629` to `1226`, ordinary-call GC-family slots from `860` to `219`,
    crossing GC registers from `9` to `3`, and assembly from `257463` to
    `255932` lines. As before, alloc-family GC-family roots rose from `757`
    to `1000`.
  - The seven-module standard `-llvm-backend` smoke
    (`cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`, `typecore`,
    `typemod`) passed, including the `typecore` case that caught an earlier
    bad root canonicalization.
  - Full boot validation still rejected it. A separate
    `_llvm_targeted_partition_boot_build` failed after `414` wrapper
    invocations with `Fatal error: allocation failure during minor GC` in
    `.ocamlcommon` and then a `.ocamlcommon` `SEGV`. I stopped the remaining
    jobs, reverted the source diagnostic, and rebuilt `llc` from restored
    sources.
  - Conclusion: partitioned complement splitting of statepoint GC values is
    still too broad or violates a hidden allocator/root invariant even when
    limited to statepoint varargs. The root counter movement is useful
    evidence, but this cannot become a performance candidate without first
    reducing to the full-boot `.ocamlcommon` failure and proving a narrower
    transformation safe.

- 2026-06-30 rejected `-split-spill-mode=default` despite focused root win:
  - Screened the saved post-RS4GC `typing/ctype.ml` IR with
    `llc -mllvm -split-spill-mode={default,size,speed}`. The current default
    for greedy is `speed`; `size` was worse (`1664` appended spill slots vs
    `1629`). `default` looked promising in isolation: appended spill slots
    dropped to `1226`, ordinary-call GC-family slots dropped from `860` to
    `219`, crossing GC registers dropped from `9` to `3`, and assembly shrank
    from `257463` to `255903` lines. The tradeoff was more alloc-family roots
    (`757` -> `1000`) and more splitting.
  - Tried a separate boot build with
    `LLVM_EXTRA_FLAGS='-mllvm -split-spill-mode=default'` using the normal
    `tools/llvm-rs4gc-llc-wrapper.sh` pipeline. Rejected on correctness before
    benchmarking: the build hit `Fatal error: allocation failure during minor
    GC` in `.ocamlcommon` and multiple `SEGV`s in `.ocamlcommon`,
    `middle_end/flambda2/types`, and `middle_end/flambda2/reaper`. I stopped
    the remaining `-j2` jobs after the repeated failure class was clear.
  - Conclusion: complement spill placement is a real control point for the
    root-pressure counters, but the global `default` mode miscompiles the
    compiler. A future fix may borrow the useful part only if it can be
    constrained to a proven-safe OxCaml/statepoint case and pass the
    seven-module workload plus full boot validation.

- 2026-06-30 stackcheck-leaf does not solve the `ctype` bottleneck:
  - Ran a focused five-sample, three-inner benchmark of the existing
    `ocamlopt.stackcheck-leaf.stage-stdlib.cache-hfsort-peep-rodata.bat.patched`
    artifact on `typing/ctype.ml` against the native-built compiler. Result:
    native median `7.478881s`, candidate median `7.409149s`, ratio `0.990676`,
    improvement `+0.93%`
    (`native-current-vs-stackcheck-leaf-stage-stdlib-cache-hfsort-peep-rodata-module-typing_ctype-samples5-inner3.json`).
  - Re-ran frametable analysis for the same artifact. The old
    `caml_llvm_call_realloc_stack` bucket is gone, but live roots are still far
    above native: native has `523867` live roots; stackcheck-leaf+BOLT has
    `699633`. Remaining deltas are mostly `noalloc+debug` stack roots
    (`310702` native vs `422229` LLVM) and `alloc+debug` roots (`205192` vs
    `263190`).
  - Conclusion: same-quality stack checks are still important, but the
    compiler-throughput blocker is not just the old stack-check path. The
    remaining `ctype` work has to reduce ordinary/debug and alloc-family root
    pressure without switching away from the arm-style in-place statepoint
    mechanism or applying a generic all-backends optimization level.

- 2026-06-30 rejected spill-weight multiplier as an LLVM-path improvement:
  - Tested a temporary hidden `llc` switch that multiplied spill weights for
    OxCaml GC virtual registers live as statepoint varargs. The goal was to
    see whether nudging greedy RA to spill those values would fold them into
    existing statepoint stack operands, as `-regalloc=basic` does in the
    diagnostic screen.
  - Screened saved post-RS4GC `typing/ctype.ml` IR at multipliers `0.75`,
    `0.5`, `0.25`, and `0.125`. The result was flat: OXSR appended-root
    counts stayed at `1629` to `1631` spill slots, with the GC-family total at
    `1618` to `1620`; assembly size changed by only a handful of lines.
  - Removed the temporary switch. Conclusion: the bad shape is not controlled
    by the final scalar spill-weight value alone. The useful contrast remains
    greedy versus basic immediately before OXSR: basic lists many more stack
    operands at statepoints, while greedy leaves long-lived GC stack homes live
    but unlisted, causing OXSR to append them. The next allocator investigation
    should look at reload folding / statepoint operand constraints or live-range
    splitting placement, not global spill-weight scaling.

- 2026-06-30 rejected OxCaml call-split-remainder root-pressure hypothesis:
  - After the `-O4` scope correction, tested another LLVM-path-only allocator
    diagnostic rather than any generic driver optimization. Added a temporary
    hidden switch around the existing greedy
    `AllowOxCamlCallSplitRemainders` path, rebuilt `llc`, and screened the
    saved post-RS4GC `typing/ctype.ml` IR with current behavior versus
    call-split remainders disabled.
  - Result: this hook is not the source of the visible `ctype` root pressure.
    Both variants reported the same `1629` statepoint spill slots, with the
    same split of `757` alloc-family, `860` ordinary-call, and `1` C-call
    GC-family slots. Regalloc bookkeeping changed slightly (`4024` splits /
    `12181` split copies at default versus `3940` / `12044` with the hook
    disabled), but root scanning did not move.
  - Removed the temporary switch after the screen. Conclusion: the long-lived
    GC-family stack homes are created by broader greedy spill placement /
    live-stack shape, not by the one extra bounded splitting stage for
    OxCaml call-crossing remainders. Keep focusing on either a narrower
    allocator/live-stack fix or an LLVM-built-binary-only BOLT/profile win.

- 2026-06-30 rejected full `-regalloc=basic` allocator diagnostic:
  - Reconfirmed the scope constraint: generic driver optimization levels such
    as `-O4` remain invalid because they also apply to the native-built
    compiler. This experiment was LLVM-path-only: change the LLVM codegen
    allocator for the LLVM-built path.
  - Screened the saved post-RS4GC `typing/ctype.ml` IR with
    `llc -O3 -regalloc=basic`. The diagnostic strongly reduced root-pressure
    counters: OXSR-appended spill slots dropped from the current `1629` to
    `74`; GC-family slots dropped from `1618` to `74`, split as `7`
    alloc-family, `66` ordinary-call, and `1` C-call. The tradeoff was much
    more allocator spilling (`2592` spill slots allocated) and larger assembly
    (`253953` lines vs about `251284` current).
  - Tried a boot build with `LLVM_EXTRA_FLAGS='-mllvm -regalloc=basic'` in a
    separate `_llvm_basicra_boot_context_build`. The build failed before a
    benchmarkable compiler existed: `.ocamlcommon` reported minor-GC allocation
    failure/SEGV, and Flambda2 native objects also took SEGVs. I stopped the
    remaining dune jobs after the repeated failure class was clear.
  - Conclusion: replacing greedy with basic is not a valid implementation
    route. It is still useful evidence: the extra AMD64 `ctype` roots are
    created by greedy register allocation / spill-slot lifetime shape before
    `OxCamlStatepointSpillRoots`, not by a missing AMD64 calling-convention
    mask or by OXSR inventing roots after the fact. The next fix should
    isolate the specific greedy decision that creates long-lived GC stack
    homes and make a narrow OxCaml-aware change that passes a full boot build.

- 2026-06-30 LLVM-path full-BOLT/ICP follow-up after excluding generic `-O4`:
  - Built a no-asserts `llvm-bolt` from the local LLVM tree and continued the
    full-BOLT path only, because generic optimization-level changes would also
    apply to the native-built compiler and are out of scope for the +6% goal.
  - Found the old `--icp-old-code-sequence` lowering was unsafe for OxCaml even
    aside from frametables: it materialized the hot target in `%r10`/`%r11`
    based only on call-instruction operands. OxCaml code can have live values in
    those registers across the callsite, so the old-code-sequence promotion can
    corrupt non-root values. The new local BOLT shape for
    `--x86-oxcaml-icp-shared-return --icp-old-code-sequence` compares the
    original indirect-call register against the hot target, emits a direct hot
    call, and keeps the fallback as `push $return; jmp *reg`.
  - Tightened `patch_ocaml_frametables.py` for this direct-hot-call shape.
    Ordinary indirect calls are now mapped from exact BAT-translated old
    indirect calls instead of broad `target=None` grouping, and ICP shared-return
    mapping prefers the fallback call's translated old call over the shared
    continuation PC. This fixed a real missing-frame failure while compiling
    `lambda/translcore.ml`: input return `0x21751ba` now maps to output return
    `0x3a90889`, and that PC is present in `camlLambda__frametable`.
  - Correctness smoke passed for the regenerated direct-ICP artifact:
    `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-oldseq-regpres-direct.bat.patched`
    reports `5.2.0+ox`, patches `224543` descriptors with `0` unresolved and
    `61` synthesized ICP descriptors, and successfully compiles the previous
    failing `lambda/translcore.ml` repro.
  - Performance is still not good enough. One-sample five-module native-mode
    compiler screens showed full direct ICP at only `+0.16%` vs native-built,
    top-callsites 10% at `+2.81%`, top-callsites 25% at `-3.02%`, and
    top-callsites 50% at `-0.57%`. These do not justify a long benchmark run
    and do not beat the current valid best, the layout-only
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at about
    `+3.77%`.
  - Conclusion: this is real full-BOLT correctness progress but not the +6%
    result. The next performance question is why even valid direct ICP loses the
    layout-only win, likely code-size/layout disruption or poor callsite
    selection. The +6% target still needs either a more selective OCaml-aware
    BOLT ICP policy, a working instrumentation/full profile path, or a backend
    root/spill improvement that raises the pre-BOLT compiler.

- 2026-06-30 LLVM-only pipeline and safe-BOLT follow-up:
  - Screened saved `typing/ctype.ml` IR with additional LLVM-only backend
    knobs. `-enable-ipra` worsened the proxy (`4675` -> `4726` total appended
    spill roots; assembly `251284` -> `251353` lines). `-enable-misched`,
    `-enable-post-misched`, both schedulers together, and `-machine-sink-bfi`
    were neutral on the saved IR stats/size. No build-worthy candidate came
    from this screen.
  - Tested safe BOLT tail-duplication variants on the relocation-enabled
    constfilter compiler. Both variants patched existing OCaml frametables and
    passed `-version`; neither used ICP or created required synthetic
    descriptors.
  - Variant using the base `ocamlopt.reloc.lbr.fdata` profile plus
    `-tail-duplication=cache` patched with zero unresolved descriptors, but
    had a weaker profile shape (`6895` profiled functions, `7` ignored profile
    objects) and benchmarked at only `+1.21%` vs native-built on the five-module
    native-mode compiler workload
    (`native-current-vs-llvm-constfilter-cache-hfsort-peep-rodata-taildup-cache-screen.json`).
  - Variant using the matching `ocamlopt.constfilter.reloc.noassert.lbr.fdata`
    profile and exact current-best block layout (`-reorder-blocks=cache`) patched
    with zero unresolved descriptors (`224542` call-site mappings, `1` BAT
    fallback), matched the current best profile shape (`7380` profiled
    functions), but benchmarked at only `+2.82%`
    (`native-current-vs-llvm-constfilter-noassert-cacheonly-hfsort-peep-rodata-taildup-cache-screen.json`).
    The `cache+` version with the same profile was also below best at `+2.71%`.
  - Conclusion: safe BOLT tail duplication is not the missing `+6%` path. The
    best valid result remains
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at `+3.77%`.
    The next viable work remains a backend/root-placement improvement or a
    real OCaml-aware BOLT frametable implementation for transformations such as
    ICP.

- 2026-06-30 LLVM-path allocator flag screen after excluding generic `-O4`:
  - Screened saved `typing/ctype.ml` post-RS4GC IR with several `llc`-only
    options. `-split-spill-mode=default` was the only promising stat change:
    total OXSR-appended spill roots moved from `4675` to `4276`, mostly by
    reducing ordinary-call GC-family slots (`3335` -> `2703`), and the emitted
    assembly shrank from `251284` to `249938` lines. `split-spill-mode=size`,
    `greedy-reverse-local-assignment`, and
    `greedy-regclass-priority-trumps-globalness` were neutral or worse.
  - Tried to build a separate split-default LLVM boot/self-stage using a
    wrapper that only appended `-mllvm -split-spill-mode=default`. The normal
    boot build failed with minor-GC allocation failures in the stage0 compiler
    while compiling boot native objects. A serialized `DUNE_BUILD_FLAGS=-j1`
    boot retry reproduced the same failure (`.ocamlcommon.objs/native/_unknown_`
    / `Fatal error: allocation failure during minor GC`), so this is not just
    parallel build pressure.
  - Conclusion: do not count or benchmark `-split-spill-mode=default` as a
    valid win. It remains an interesting diagnostic hint that spill partitioning
    affects `ctype` root pressure, but the actual fix needs to be a narrower
    OxCaml-aware allocator/root-placement change that preserves boot build
    correctness.

- 2026-06-30 LLVM-path backend ablation after the `-O4` correction:
  - Rechecked the safe BOLT `-reg-reassign` artifact before benchmarking it.
    Both `ocamlopt.constfilter.cache-hfsort-peep-rodata-regreassign.bat.patched`
    and the older `frameopt-regreassign` artifact abort immediately on
    `-version` with `Fatal error: allocation failure during minor GC`, so
    BOLT register reassignment remains unsafe for this compiler binary and is
    not a benchmark candidate.
  - Tested a focused RA spill-weight hypothesis locally: add a hidden
    `-oxcaml-statepoint-vararg-weight-multiplier` knob that boosts spill
    weights for virtual registers used as statepoint GC operands, then compile
    the saved `typing/ctype.ml` LLVM IR with multipliers `2`, `4`, and `8`.
    The source edit was reverted after the experiment.
  - Result: no useful movement. Baseline `ctype` stats are `407`
    fixup spill slots, `798` fixup-spilled registers, `1618` GC-family
    appended slot roots, `11` reload-fed sibling roots, `9` register roots,
    and `1629` total appended slots. Multipliers `4` and `8` still had `407`
    fixup spill slots and `798` spilled registers, but changed the root mix to
    `1619` GC-family slots, `8` register roots, and `1630` total appended
    slots; multiplier `2` was similarly neutral/slightly worse (`1631` total).
  - Conclusion: simply biasing RA to keep statepoint operands in registers
    does not address the long-lived GC-family stack homes in `ctype`. The next
    backend path should inspect where those stack slots are created/split, not
    just adjust their generic spill weight. The best valid measured artifact
    remains `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at
    `+3.77%` over the native-built compiler.

- 2026-06-30 LLVM-path BOLT follow-up after the `-O4` scope correction:
  - Confirmed again that the target must be an LLVM-path improvement, not a
    generic optimization-level change that a native-built compiler could also
    use.
  - Prototyped a direct shared-return BOLT ICP shape locally:
    promoted direct arms used `push $merge_return; jmp target` so every arm
    shared the original descriptor-bearing return PC. Built `llvm-bolt`
    successfully, then used a no-asserts BOLT build because the assertions
    build hits the known pre-ICP CFI stack assertion in this binary.
    BOLT completed with `-indirect-call-promotion=calls` and
    `--x86-oxcaml-icp-shared-return`; frametable patching reported
    `patched 224543`, `unresolved 0`, and the artifact passed `-version`,
    `-config`, and a small native compile smoke. Artifact:
    `bolt_compiler_20260629/ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-sharedret-v3.bat.patched`.
  - Rejected that source/code-shape change for performance. The full
    seven-sample / three-inner-repetition five-module benchmark gave native
    median `27.025927s`, candidate median `28.126696s`, ratio `1.040730`,
    i.e. `-4.07%` in
    `native-current-vs-llvm-constfilter-cache-hfsort-peep-rodata-icp-sharedret-v3-inner3.json`.
    The source and patcher edits for this experiment were removed; do not
    carry the `push; jmp direct_target` shape forward as a performance fix.
    The better full-BOLT path is exact frametable synthesis/mapping for real
    promoted direct calls, preserving BOLT's fast direct-call shape.
  - Also screened the apparent data-reordering gap by trying the safe BOLT
    recipe with `-jump-tables=basic -reorder-data=.rodata
    -reorder-data-algo=funcs`. BOLT still forced `jump-tables=move` for a PIC
    jump table and warned that data reordering requires basic jump-table
    support, so no useful data-reorder artifact was produced. Log:
    `bolt_compiler_20260629/constfilter-cache-hfsort-peep-rodata-jtbasic-data.log`.
  - Investigated the real BOLT ICP direct-call shape instead of the rejected
    `push; jmp` shared-return shape. Plain BOLT ICP produced
    `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-calls-v4.bat.bolt`
    with about `79.2%` of profiled indirect callsites optimized, but the
    existing frametable patcher missed BOLT-created direct-call return PCs
    such as `0x3bb5dc3` in `List.concat_map`, causing
    `caml_scan_stack: missing frame descriptor`.
  - Extended the local patcher experimentally to synthesize descriptors for
    promoted direct-call returns by cloning the fallback indirect-call
    descriptor when the fallback return jumps to the same post-call
    continuation. This produced
    `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-calls-v4.synth4.bat.patched`
    with `224543` ordinary descriptors patched, `0` unresolved descriptors,
    and `312` synthesized ICP descriptors. It passed `-version` and a small
    native compile smoke.
  - Rejected that normal-ICP synthesis as not yet correct. The five-module
    smoke failed compiling `backend/llvm/llvmize.ml` with
    `Invalid_argument("hash: mixed block value")`, consistent with a wrong
    root map rather than a missing descriptor. Ablations skipping either half
    of the synthesized descriptor set immediately produced missing-frame
    aborts (`0x3b460f7`, `0x3bb5dc3`, etc.), so many synthesized descriptors
    are genuinely needed; the current problem is exact root-layout correctness
    for BOLT-created callsites, not simply over-synthesizing unused entries.
    Do not benchmark normal ICP until the descriptor model can survive real
    compiler-module compilation.
  - Current measured best is unchanged:
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at `+3.77%`.
    The viable LLVM-only path is now backend root/spill precision, or a much
    stronger OCaml-aware BOLT frametable implementation for new promoted call
    return PCs that proves live-root equivalence instead of cloning
    descriptors heuristically.
  - Rechecked the existing BOLT instrumentation artifacts after the scope
    correction. Both `ocamlopt.instrumented` and
    `ocamlopt.instrumented.skipgc` still segfault immediately on `-version`.
    That confirms instrumented full-BOLT profiles are not available yet; the
    startup blocker remains OCaml frame/GC metadata for BOLT-inserted
    instrumentation code, not the earlier runtime symbol-link issue.

2026-06-29 BOLT ICP shared-return prototype: this is an LLVM/BOLT-path-only
experiment, not a generic `-O4` style change. Added a hidden X86 BOLT mode
prototype, `--x86-oxcaml-icp-shared-return`, intended to keep OxCaml GC frame
descriptors valid when BOLT does indirect-call promotion. The useful machine
code shape for cold fallback calls is:
`push $shared_return; jmp *callee`, where `shared_return` is a continuation PC
that already has the original callsite's frame descriptor. The patcher now
detects this shape and maps the old descriptor using the converted indirect
`jmp`, whose BAT entry maps back to the original indirect call. This fixed the
first full-ICP startup missing-descriptor class during the prototype: a
fallback-only patched artifact reached `-version` once `224444` frame
descriptors mapped through call sites and only `99` used generic BAT fallback.
The current patcher deliberately no longer exits successfully for that artifact,
because startup success is too weak while promoted direct-call return PCs still
lack descriptors.

The prototype is not yet benchmarkable. Multi-target ICP is unsafe without
descriptor duplication, so the BOLT mode now skips non-tail multi-target
callsites; this reduces optimized ICP callsites from `76.4%` to `71.5%` while
leaving most ICP active. The hidden mode also rejects memory-form indirect
calls for now; `call *mem` is not generally equivalent to `push return; jmp
*mem` when the memory operand is `%rsp`-relative. The remaining correctness
blocker is normal promoted direct calls: BOLT emits a direct call that returns
to an intermediate PC and then jumps to the shared continuation. Re-running the
current patcher on the fallback-only BOLT output now exits nonzero with
`unresolved 16` candidate promoted-direct hazards. One proven site is
`camlCtype__with_local_level_19_945_code`: `call caml_tuplify6` returns to
`0x3ba532f`, then jumps to descriptor-bearing `0x3ba5320`. This is the same
site that made the real compiler-module smoke fail while compiling
`backend/llvm/llvmize.ml`. Therefore full ICP needs real descriptor
synthesis/duplication for promoted direct-call return PCs, or a BOLT call shape
that preserves a single descriptor-bearing return PC.

Patcher work in
`bolt_compiler_20260629/patch_ocaml_frametables.py` now detects
`push $return; jmp *callee` fallback sites and maps sparse single-target
promotions inside large functions by using the converted indirect jump's BAT
identity. It also rejects promoted direct-call return PCs in the same old-call
interval, because those need new descriptors and cannot be made correct by
moving the old descriptor. This reject check is deliberately fail-closed and
can over-approximate; it is a guard against benchmarking unsafe artifacts, not
a proof that every reported site is independently bad. The earlier approximate
direct-call fallback and
address-order zip heuristics were removed after review because they could
silently assign a descriptor to the wrong PC. A direct promoted-call
`push shared_return; jmp direct_target` source experiment was also rejected:
the generated ctype site pushed the shared return but jumped back to the merge
block instead of to `caml_tuplify6`, so the source was restored to
fallback-only. Do not benchmark the ICP shared-return artifacts until
`-version` and real compiler-module compile smoke tests both pass.

2026-06-29 LLVM-only scalar spill-fusing follow-up: tested a narrower hidden
X86 `llc` flag that disabled scalar integer ALU spill folding only in OxCaml
functions after a statepoint in the same machine block. It fixed the focused
dynamic GC loop shape locally and could build a full LLVM-stage compiler when
bootstrapped from the clean native install
`_native_current_install` (`main fresh ir: 1114`). The existing
`_install/bin/ocamlopt.opt` is not a trustworthy bootstrap for this
validation: it can abort before wrapper invocation while compiling
`otherlibs/dynlink/parser.ml` with `Fatal error: allocation failure during
minor GC`; the same action succeeds under `_native_current_install`.

Result: rejected for the compiler-throughput goal. Benchmarking the clean
native-bootstrapped LLVM-stage compiler against the native-built compiler,
both compiling in normal native mode over the five standard modules with
`samples=7`, `inner_repetitions=3`, gave native median `27.068969s`, candidate
median `28.667318s`, ratio `1.059047`, i.e. `-5.90%`. Artifact:
`bolt_compiler_20260629/native-current-vs-llvm-scalaralu-nativeboot-benchmark.json`.
The experimental LLVM source/test changes were removed. The current best valid
full-workload LLVM-path result remains the safe BOLT layout binary
`ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at about `+3.77%`;
the remaining path to the required `+6%` is still real OCaml-aware BOLT
frametable support for transformations such as ICP, or a precise AMD64 LLVM
codegen improvement that helps the compiler workload rather than the isolated
loop benchmark.

2026-06-29 scope correction: do not count generic optimization-level changes
such as `-O4` toward the compiler-performance goal. Those would apply equally
to a native-built compiler, so the remaining work must improve the LLVM-built
path specifically: backend code generation, LLVM/BOLT handling, LLVM-only
profile/layout work, or another change that is not available to the native
build under the same benchmark setup.

2026-06-29 ctype-focused BOLT experiment: the current best valid compiler
binary remains
`ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched`, with
`+3.77%` on the five-module compiler-throughput workload
(`samples=7`, `inner_repetitions=3`). Per-module reruns against the same
native-built baseline show the blocker is `typing/ctype.ml`: `cfg_selectgen`
`+3.91%`, `llvmize` `+4.63%`, `translcore` `+4.94%`, `ctype` only `+0.87%`,
and `env` `+4.13%`. Supporting artifacts are
`native-current-vs-llvm-constfilter-cache-hfsort-peep-rodata-module-*-inner3.json`.
A best-candidate perf profile for `ctype` is heavily runtime/GC dominated
(`do_some_marking` `13.41%`, `oldify_one` `7.90%`, `clear_garbage` `3.76%`,
`caml_modify` `2.98%`), and hot helper disassembly still shows LLVM-generated
AMD64 code using larger frames/root-slot traffic in some functions than the
native backend shape. Supporting artifacts are under
`best_peep_perf_profiles_20260629/`, especially
`best-ctype-symbol-report-simple.txt`,
`native-ctype-symbol-report-simple.txt`, and `asm/`.

To test whether this was mostly a BOLT profile-coverage issue, collected a
new LBR profile from `ocamlopt.constfilter.reloc` compiling only
`typing/ctype.ml` for seven repetitions:
`ocamlopt.constfilter.reloc.ctype7.lbr.perf.data` and
`ocamlopt.constfilter.reloc.ctype7.lbr.fdata`. `perf2bolt` read 66,734
samples and 1,066,577 LBR entries. Rebuilt with the same safe BOLT recipe
(`-reorder-blocks=cache`, `-reorder-functions=hfsort`, `-peepholes=all`,
`-reorder-data=.rodata`, `--enable-bat`) and frame-table patched
`ocamlopt.constfilter.ctype7-cache-hfsort-peep-rodata.bat.patched`
successfully (`patched 224543`, `unresolved 0`). The binary passed `-version`
and direct compile smoke tests for both `typing/ctype.ml` and
`backend/llvm/llvmize.ml`.

Result: this focused profile improves the holdout but is not a keeper. The
`ctype`-only benchmark improves to `+2.23%`
(`native-current-vs-llvm-constfilter-ctype7-cache-hfsort-peep-rodata-module-typing_ctype-inner3.json`),
but the full five-module benchmark is only `+3.34%`
(`native-current-vs-llvm-constfilter-ctype7-cache-hfsort-peep-rodata-inner3.json`),
worse than the existing `+3.77%` best. Conclusion: BOLT profile specialization
can move `ctype`, but it trades away broader layout wins. The next plausible
LLVM-only improvement is not `-O4` or another generic optimization level; it is
either real OCaml/BOLT metadata support for transformations like ICP that
create new managed callsites, or a precise AMD64 LLVM codegen fix for the
extra statepoint/call-adjacent stack traffic and root-slot/frame shape.

Also tested a normal-profile merge of the existing broad five-module fdata
(`ocamlopt.constfilter.reloc.noassert.lbr.fdata`) and the new `ctype7` fdata
(`ocamlopt.constfilter.reloc.ctype7.lbr.fdata`), producing
`ocamlopt.constfilter.reloc.noassert-plus-ctype7.lbr.fdata`; merge log:
`merge-noassert-plus-ctype7.log`. This avoids the previously rejected mixed
`boltedcollection` post-BOLT merge issue because both inputs are pre-BOLT
profiles for `ocamlopt.constfilter.reloc`. The merged profile BOLT binary
`ocamlopt.constfilter.noassert-plus-ctype7-cache-hfsort-peep-rodata.bat.patched`
patched cleanly (`patched 224543`, `unresolved 0`) and passed `-version` plus
direct `ctype`/`llvmize` compile smoke tests, but a short full-workload screen
was only `+1.52%`
(`native-current-vs-llvm-constfilter-noassert-plus-ctype7-cache-hfsort-peep-rodata-screen.json`).
Rejected without a stronger run.

2026-06-29 unroll follow-up: reran the no-loop-unrolling idea through the real
LLVM backend wrapper rather than relying on the earlier invalid benchmark.
Global `--disable-loop-unrolling` for the main compiler rebuild is rejected:
the wrapper logs show 271 optimizer invocations / 271 fresh IR compilations,
then the build failed while compiling `otherlibs/dynlink` and `.ocamlcommon`
with `allocation failure during minor GC` / `SEGV`; no
`_llvm_nounroll_clean_install/bin/ocamlopt.opt` was produced. Whole-runtime
stdlib no-loop is also rejected: the runtime stdlib build itself completed
with 74 fresh IR compilations, but a normal main rebuild against it failed
with the same minor-GC/SEGV pattern and produced no install. A narrower
`stdlib__Hashtbl.ll`-only no-loop wrapper matched exactly that stdlib IR file
once, but a normal main rebuild against the resulting stdlib still failed with
minor-GC allocation failures in `otherlibs/dynlink` and `.ocamlcommon`, again
with no installed compiler. Conclusion: the Hashtbl full-unroll code-size
pathology is real, but disabling loop unrolling, even only for Hashtbl in the
stdlib, is not a safe compiler-throughput improvement in this pipeline.
Artifacts are under
`agent-state/test-suite-29e4cd/unroll_investigation_20260629/`, especially
`build_nounroll_clean_main.log`, `build_nounroll_stdlib_main_normal.log`,
`build_hashtbl_nounroll_main_normal.log`, and the corresponding
`llvm-wrapper-*` / `opt-*` logs.

2026-06-29 BOLT ICP/inlining follow-up: further LLVM-path BOLT variants still
do not reach the `+6%` native-built-compiler target. `--icp-inline` reduced
the number of optimized ICP callsites versus full ICP (`26.0%` of profiled
indirect callsites rather than `76.4%`), but it still failed startup with
`caml_scan_stack: missing frame descriptor retaddr=0x3c5ebdb` after normal
frametable patching. Post-BOLT guard ablations that forced 79 ordinary OCaml
promoted-call fast paths to the original indirect fallback still failed
compiling `backend/llvm/llvmize.ml` with `allocation failure during minor GC`;
patching 12 remaining register-indirect fallback cases and then the final 4
C/runtime hook fast paths did not fix the corruption. Conclusion: this ICP
output is not recoverable by simply disabling recognizable fast paths after
BOLT; safe ICP needs either BOLT-side filtering before transformation or a real
OCaml-aware metadata model.

BOLT `-inline-small-functions` is safe but not useful enough. With
`-inline-small-functions-bytes=16`, BOLT inlined 1,099 calls at 645 callsites,
patched all 224,543 frame descriptors, passed `-version`, and compiled
`backend/llvm/llvmize.ml`, but screened at only `+3.69%`:
`native-current-vs-llvm-constfilter-cache-hfsort-peep-rodata-inline-small16-screen.json`.
With threshold `32`, BOLT inlined 9,905 calls at 1,447 callsites and also
passed the same smoke tests, but screened at only `+3.36%`:
`native-current-vs-llvm-constfilter-cache-hfsort-peep-rodata-inline-small32-screen.json`.
The current best stronger result remains the non-inlining
`cache-hfsort-peep-rodata` BOLT binary at `+3.77%`.

2026-06-29 BOLT ICP frametable investigation update: the current
LLVM-built-compiler-only BOLT path still does not meet the required `+6%`
target over the native-built compiler. Full BOLT indirect-call promotion can
create new promoted direct-call return PCs for OCaml-managed calls. Existing
descriptor retaddr patching is insufficient because those new return PCs did
not exist in the input frametables. Example failure:
`camlStdlib__List__concat_map_64_160_code` gained a promoted direct call whose
return PC `0x3c5a81b` had no descriptor, while the original fallback indirect
call return PC had one. Prototype ELF-level synthetic frametable growth can
make startup pass by redirecting one zero-count frametable pointer to appended
descriptors, but two tested descriptor-selection policies are not safe enough:
a broad callsite-matching synthesizer produced 12,228 new descriptors and then
failed compiling `backend/llvm/llvmize.ml` with `allocation failure during
minor GC`; a conservative direct-call/fallback-jump pattern produced 195 new
descriptors and still corrupted GC once the demand-driven set included the
descriptor for return PC `0x3b9993a`. This indicates that safe ICP support
needs a principled OCaml/BOLT frametable model for BOLT-created callsites, not
blind descriptor cloning.

Also tested a restricted `-indirect-call-promotion=calls
-icp-top-callsites=10` build. BOLT reported zero optimized callsites, so it
needed no synthetic frametable entries and passed both `-version` and a direct
`backend/llvm/llvmize.ml` compile. Benchmarking against the native-built
compiler on the five-module compiler-throughput workload gave only `+3.40%`
with `samples=7, inner_repetitions=3`
(`native-current-vs-llvm-constfilter-cache-hfsort-peep-rodata-icp-top10-inner3.json`),
worse than the existing best `cache-hfsort-peep-rodata` result (`+3.77%`).
Conclusion: ICP remains potentially LLVM-path-specific, but the safe top10
variant is effectively a no-op, and the real ICP variants are blocked on
correct frametable synthesis.

2026-06-29 rejected LLVM-path experiment: a targeted X86 hidden-flag prototype
that disabled folded stack-slot reloads after OxCaml statepoints fixed the
focused `loop_invariant_gc_across_call_dynamic_reps` shape locally (normal real
wrapper full-size run about `1.51x` LLVM/native; prototype about `1.02x` to
`1.09x`, depending on sample count). This confirmed that the microbenchmark
slowdown is post-statepoint X86 spill folding, not a frontend-root or generic
optimization-level issue. However, the prototype is not viable as implemented:
exception one-sample screening was mixed, benchmarksgame was mostly neutral,
and a real guarded self-stage boot build reproducibly crashed `llc` in greedy
register allocation/InlineSpiller while compiling `tools/simdgen/simdgen.ml`
and `external/owee/owee_elf_notes.ml` (`MachineOperand::getReg()` assertion)
even after excluding statepoint/patchpoint/stackmap pseudos. The experimental
source change and temporary wrappers were removed; normal `make -C
_build/llvm-tools -j2 llc` passes again. Any future fix must be more precise
than blocking broad post-statepoint folding, and must pass a real LLVM build
before benchmarking.

2026-06-29 BOLT follow-up: tested additional LLVM-built-compiler-only BOLT
variants; none reaches the `+6%` target, and none beats the existing best
`cache-hfsort-peep-rodata` result (`+3.77%`) on comparable inner-repetition
runs. Indirect-call promotion is not safe with the current OCaml frametable
patcher: both
`-indirect-call-promotion=calls` and the same with `-icp-old-code-sequence`
patched all existing descriptors but failed immediately at startup with
`caml_scan_stack: missing frame descriptor`. The missing PCs are newly-created
promoted-call return addresses, e.g. in
`camlStdlib__List__concat_map_64_160_code`; the patcher rewrites old
descriptors but cannot synthesize extra descriptors for BOLT-added callsites.
So ICP requires real frame-table growth/synthesis before it can be benchmarked.
Safe layout-only variants were neutral or worse than best:
`cache-hfsortplus-peep-rodata` starts and measured `+3.82%` on a short screen
but only `+3.35%` on `samples=5, inner=3`; `blocknormal-hfsort-peep-rodata`
starts but screened at `+3.21%`. Artifacts live under
`agent-state/test-suite-29e4cd/bolt_compiler_20260629/`, including
`native-current-vs-llvm-constfilter-cache-hfsortplus-peep-rodata-inner3.json`,
`native-current-vs-llvm-constfilter-blocknormal-hfsort-peep-rodata-screen.json`,
and the corresponding `constfilter-...icp...` logs.

2026-06-29 later BOLT follow-up: tested post-layout profile refinement and
register reassignment; neither is a keeper. Profiling the current best BOLTed
compiler worked. Artifacts live under
`agent-state/test-suite-29e4cd/bolt_compiler_20260629/`: `perf2bolt`
recognized a `boltedcollection` and emitted
`ocamlopt.constfilter.cache-hfsort-peep-rodata.profiled.lbr.fdata`. Reapplying
the best safe BOLT recipe to the original relocatable compiler with that
translated profile produced a runnable binary with complete frametable patching
(`patched 224543`, `unresolved 0`), but it screened at only `+3.70%`:
`native-current-vs-llvm-constfilter-refined-cache-hfsort-peep-rodata-screen.json`.
Merging the original and translated post-BOLT profiles is not valid as-is:
`merge-fdata` rejects mixed normal/`boltedcollection` inputs, and stripping the
marker makes BOLT report `98.0%` of samples in stale functions; that binary
starts but screens at `-0.49%` in
`native-current-vs-llvm-constfilter-original-refined-noheader-cache-hfsort-peep-rodata-screen.json`.
BOLT `-reg-reassign` is unsafe for this compiler binary: both the existing
`cache-hfsort-peep-rodata-frameopt-regreassign` artifact and a new isolated
`cache-hfsort-peep-rodata-regreassign` build patch all existing frame
descriptors but abort on `-version` with `allocation failure during minor GC`.
Current best stronger `inner_repetitions=3` result remains
`cache-hfsort-peep-rodata` at `+3.77%`; the likely next LLVM-only work is either
real frametable synthesis for BOLT-created callsites so ICP can be measured
safely, or a precise LLVM codegen improvement rather than more generic BOLT
layout variants.

2026-06-29 compiler-performance investigation update: the large
`Hashtbl.create`/capacity-growth code-size pathology is real, but the tested
global full-unroll cap is not a useful compiler-throughput lever. Local,
currently untracked workspace artifacts include captured `stdlib/hashtbl.ml`
LLVM IR in
`agent-state/test-suite-29e4cd/unroll_investigation_20260629/hashtbl_compile/`;
rerun the investigation rather than relying on those paths after a fresh clone.
`LoopFullUnrollPass` fully unrolls the bounded doubling loops (`L125` in
`camlHashtbl__create_inner_7_118_code`, `L4212` in
`camlHashtbl__create_83_232_code`), creating the long compare ladder. Local
ablations fix the assembly shape: `--disable-loop-unrolling` shrinks
`create_83` from 208 assembly lines to 58, and
`--unroll-full-max-count=41` does the same while cap 48 allows the ladder.
However, the whole-compiler build evidence only rejects the narrower full-unroll
cap 41, not no-loop-unroll generally: the direct no-loop-unroll build failed
with a minor-GC allocation failure, and the later install that measured `-6.45%`
logged zero wrapper invocations, so do not use that benchmark as evidence about
LLVM no-loop-unroll throughput. The full-unroll cap 41 build completed with 148
runtime and 2228 main wrapper invocations, smoked, and using the real binary
still measured `-5.21%` vs native in the local workspace artifact
(`native-current-vs-llvm-fullcap41-real-screen.json`). The same screen with
the current LLVM compiler is roughly parity with native (`+0.15%`,
`native-current-vs-llvm-constfilter-current-screen-rerun.json`). Do not pursue
a global full-unroll cap; no-loop-unroll would need a clean LLVM-path build
before it can be judged. Any future fix for this code-size issue must be more
surgical and rebenchmarked against the compiler workload before keeping.

Last updated: 2026-06-29. Important correction: the broad benchmark run in
`spill_fusing_effect/` used the temporary `llc-wrapper.sh`, which fed raw
pre-optimization LLVM IR directly to `llc`. That is not the project pipeline:
`tools/llvm-rs4gc-llc-wrapper.sh` runs
`default<O3>,rewrite-statepoints-for-gc,verify` before `llc`. The raw-wrapper
run made `matmul` look catastrophically slow because 211 frontend-style
`alloca`s survived to codegen. With the real wrapper, fresh `SAMPLES=7`
minibench results are `matmul` native `0.1158s`, LLVM `0.1062s`, ratio
`0.9171`, and `matmul_transposed` native `0.0920s`, LLVM `0.0659s`, ratio
`0.7162`. Treat `matmul` as roughly parity given native-sample variance; treat
the broad `spill_fusing_effect/` tables as invalid for
LLVM-vs-native performance and rerun any global spill-fusing ablation through
the real wrapper.

Full corrected rerun with the real wrapper (`SAMPLES=7,WARMUPS=2`) is saved in
`agent-state/test-suite-29e4cd/real_wrapper_full_bench_20260629/`. Across the
84 broad benchmark cases (minibench, benchmarksgame, exception microprobe),
total LLVM/native runtime ratio is `0.8822`, geomean `0.8840`, with 21 cases
slower than native, 10 above 1.05x, and 7 above 1.10x. Including the four
focused loop-invariant probes gives 88 cases total, total ratio `0.8858`,
geomean `0.8972`, 23 slower, 12 above 1.05x, and 9 above 1.10x. Largest broad
slowdowns are `exception/closure_call_many_handler_live_roots_raise` `1.1912x`,
`minibench/hash_batch_murmur_mix` `1.1807x`,
`exception/raise_caught_cross_function` `1.1734x`, `benchmarksgame/nbody_1`
`1.1401x`, and `exception/raise_payload_caught_cross_function` `1.1256x`.
The focused GC loop-invariant probes remain the largest overall slowdowns:
dynamic reps `1.5823x`, fixed reps `1.5735x`. Summary:
`agent-state/test-suite-29e4cd/real_wrapper_full_bench_20260629/summary.md`.

Slowdown deep dive completed for the top corrected cases. Artifacts and report:
`agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/report.md`.
Main classifications: loop-invariant GC probe is post-RA X86 spill-fusing of
scalar loop state around statepoints; exception slowdowns are
invoke/landingpad/trap-loop state placement rather than a pure GC root bug;
`nbody_1` is largely a float `sqrt` lowering gap (`sqrt@PLT` vs native
`vsqrtsd`, patched assembly closes about half the gap); `finance_greeks_pnl` is
a smaller call/statepoint-adjacent spill scheduling issue; `matmul` is healthy
under the corrected pipeline.

Current AMD64 investigation: the
`loop_invariant_gc_across_call_dynamic_reps` slowdown is caused by X86
spill-fusing of scalar reloads after an OxCaml statepoint/call, not by the GC
root stack-slot mechanism and not primarily by the chosen physical register
names. Pre-regalloc MIR is reasonable; greedy/inline-spiller split the scalar
live-through-call values, create ordinary scalar spill slots, and X86
`foldMemoryOperandImpl` folds the post-call accumulator reload into memory
arithmetic. `-disable-spill-fusing` fixes the focused microbenchmark
(`~0.410s -> ~0.237s` at `reps=20`; both no-hoist/no-fusing `~0.230s`), while
`-disable-spill-hoist` alone does not (`~0.408s`). Next step: rerun the
global spill-fusing ablation through `tools/llvm-rs4gc-llc-wrapper.sh`, then
prototype a narrow X86/OxCaml predicate that suppresses only harmful
statepoint-adjacent scalar spill folding, and add a focused MIR test that
still verifies GC roots remain stack roots. Detailed plan:
`amd64-arm-parity-plan.md`; raw/summary benchmark artifacts:
`agent-state/test-suite-29e4cd/spill_fusing_effect/summary.md` and
`summary.json` are raw-`llc` artifacts, not valid project-pipeline performance
results.

Previous status (2026-06-12): THE LATENT FLIP MISCOMPILE IS FIXED (7 bugs
total) AND THE FULL GATE IS GREEN: stage1+stage2 builds, boot-flip 3/3,
stage1-binary flip-stress 16/16 module runs (typecore ctype typedecl
env typeclass parmatch matching translcore btype subst printtyp),
SELF_STAGE=2 ocamltest 6756/0 (allocation.ml expect promoted for
slot-only spill codegen), lit = known pre-existing failures only
(statepoint-call-lowering.ll verified failing at baseline 57e9764b3c
too), minibench geomean 0.8870 vs native (no regression vs 0.9064
prior slot-only / 0.886 vreg-era). All changes uncommitted.

## BUG 6: mixed spill slots after StackSlotColoring (2026-06-12)

The stage1-binary flip-stress failures (typecore/ctype/typedecl/env)
survived the five fixes below. Forensics (watchpoint-grade, see
/tmp/gcscan2.py `gcl`/`gcwatch`) pinned the stale root to
Closure_conversion.cont_96_350: the same young value V0 sat in listed
sp+0x80 (GC updated it), in UNLISTED RA family slots sp+0x20/sp+0x70
(%stack.32 — read later, the crash chain), and in the gc_regs bucket
slot of dead x1 (the earlier "heap holder" red herring — buckets are
malloc'd near the domain). The pass skipped %stack.32 as "non-value"
because ONE of the slot's stores was raw: StackSlotColoring runs before
the pass and merges different vregs' disjoint ranges into one FI (and
collapses LiveStacks VNIs), so the per-FI "all stores are values"
summary is structurally wrong. Fixed in OxCamlStatepointSpillRoots:
per-query REACHING-STORE value-ness (block-local last store, else AND
over pred-exit states, memoized/optimistic on cycles), with new value
evidence: ValueHome-store seeding (covers STRXpre alloc write-backs),
a new MOOxCamlGCValue MMO flag set by TargetLoweringBase for IR
ptr-addrspace(1) loads/stores, entry physreg arg spills via MRI
liveins, direct-callee statepoint-result return-type lookup, consumer
evidence via seeded reloads, and odd immediates as tagged scalars.
Corpus (ccmain2/ltf/parmatch/typecore IR with EXACT build flags from
clang-wrapper.log — llc without -ffixed-x15/x26 hides everything):
remaining skips all verified benign (i64-typed immediates, LOADgot
statics). Parked: port GCValueness into the verifier (it now FPs on
entry-init-extended live ranges); statepoint-call-lowering.ll lit
failure needs a pre/post check.

BUG 7 (same day): the BUG 6 fix moved the crash INTO the GC and
exposed that register roots had been appended at C-CALL statepoints,
where they are unresolvable: only the alloc-family runtime entries run
SAVE_ALL_REGS, and RESTORE frees the bucket without clearing
Caml_state->gc_regs, so the walk reads a FREED bucket (crash:
Regalloc_irc.run's `bl caml_c_call` descriptor listed reg16=x19; an
allocating C primitive GC'd; oldify(stale residue) SIGBUS'd in
get_header_val). Fixed: register appends and the verifier's real
register findings are gated on alloc-family statepoints (regmask
preserves X0). Validation cascade running.

## RA-derived roots, step 0 + residual root fixes (2026-06-11, uncommitted)

Built the gc bit on virtual registers (MachineRegisterInfo, seeded at
statepoint emission, inherited through splits, OR'd by the coalescer) and
the flag-gated post-RA `OxCamlGCRootVerifier` pass. First corpus run (841
stashed modules) found residual root-listing bugs; both classes fixed in
OxCamlStatepointSpillRoots the same metadata-only way:
- gc-bit-based slot families (RA re-spills of ISel-slot-resident values
  created unlisted second slots): 198 violations -> 0.
- register second locations (an RA/pre-RA copy of a value that IS listed
  at the statepoint survives in a preserved register unlisted, e.g.
  Misc.loop_77 %173/$x13): now appended as in-place register roots
  (`-oxcaml-statepoint-register-roots`): 94 violations -> 0.
The earlier "llvmize ptrtoint store" suspicion was RETRACTED: that value
is statically a tagged immediate; the verifier now separates that shape
(value-connectivity union-find) and reports it as info only.
Corpus after fixes: 0 slot / 0 register / 0 clobbered violations.
LATER THE SAME DAY: per-register taint proved unsound for LISTING (raw
ranges of coalesced vregs would be handed to the GC as roots), so the
pass gained a flow-sensitive per-VNI value analysis (GCValueness) — only
provably-value contents are listed. AND: stage builds exposed a
DETERMINISTIC PRE-EXISTING latent miscompile (typecore + young-flip
SIGBUS, crashes with baseline/before-5/current clang alike, native
passes — NOT caused by this week's work; every earlier green gate
shipped it). Root cause open; this now gates landing. Details:
RA_DERIVED_ROOTS_PLAN.md (step 0 sections + "LATENT PRE-EXISTING
MISCOMPILE FOUND").
CONTINUED (same day, root-cause marathon): the latent crash decomposed
into FIVE compiler bugs, all root-caused: (1) exnroot-homed values
RA-re-spilled into unlisted slots [fixed: value-home FIs seed family +
value-ness]; (2) loop-carried listed slots uninitialized on iteration 1
[fixed: dominance check + entry init]; (3) the cont pattern — tied-def
vreg lowering at alloc statepoints creates unlisted live pool-slot
copies, unfixable post hoc [decision: MaxVRegPtrs=0 default for oxcaml
until in-place statepoints (plan step 1-2); register lowering stays
available via -max-registers-for-gc-values]; (4) DETERMINISTIC:
StatepointStableRootHomes seed stores on critical edges clobber the
home (Parmatch.pressure_variants -> bogus non-exhaustiveness errors;
broke every -warn-error build under slot-only mode) [fixed: homes
require non-critical seed edges]; (5) home FIs were in the statepoint
slot pool and could be handed to unrelated values [fixed: not pooled].
Full forensics, repro recipes and hunt methods in
RA_DERIVED_ROOTS_PLAN.md.

## Entry-cost codegen fixes (2026-06-11)

Three systematic per-function/per-callsite costs found by reading hot asm vs
native (boyer truep, kb match_rec, micro try/raise probes), all fixed:

1. Split prologue (`sub sp,#16; str x30; sub sp,#N` -> one
   `sub sp,#16+N; str x30,[sp,#N+8]`): `CombineSPBump` is now allowed for
   OxCaml functions that need no PROLOGUE stack check (the check must see SP
   before any allocation). LR stays at the frame top via the existing CSR
   offset fixup; the epilogue already combined.
   (AArch64FrameLowering.cpp)
2. Ordinary stack checks read SP via `llvm.read_register` instead of inline
   asm (llvmize.ml, AArch64 now matches x86), and a new
   `AArch64MIPeepholeOpt::visitCmpWithSPCopy` folds `COPY $sp` + `SUBS` into
   `cmp sp, xN` (guarded: same block, no SP writes/calls between). Saves an
   instruction plus an inline-asm scheduling barrier at ~every function
   entry (372/725 functions in the minibench suite).
3. C-wrapper call arguments with no use after the call are no longer
   caller-rooted (`-rs4gc-oxcaml-root-dead-c-call-args` restores the old
   behaviour). Callees root their own parameters per the OCaml FFI contract
   (CAMLparam), exactly as with the native compiler; caller-rooting forced a
   dead stack store per C call site (visible in every compare/hash loop).
   `HasUseAfterCall` also refined: a value defined in the call's own block
   before the call is dead along the back edge (single-entry blocks),
   removing the loop-recurrence false positive. Two RS4GC lit tests updated
   to check both modes.

Results: micro 44-case geomean 0.662 -> 0.652, minibench geomean
0.906 -> 0.886 (no case above bdd's 1.001), compiler bench module-median
ratio 0.9732 -> 0.9715 (round-total 0.9740 -> 0.9699) against the identical
native baseline binary. Validation: fresh stage1+stage2 builds, SELF_STAGE=2
ocamltest `6756 passed / 0 failed`, lit at the known pre-existing set,
parser.pp.ml repro + OXCAML_YOUNG_FLIP gates clean.

Full investigation + staged plan for RA-derived GC roots (in-place
statepoint lowering, gc bit on vregs, verifier): see
`agent-state/test-suite-29e4cd/RA_DERIVED_ROOTS_PLAN.md`.

Identified but NOT yet implemented (next): redundant statepoint re-spills of
loop-carried GC values - ISel's `findPreviousSpillSlot` gives up on PHIs
whose inputs map to different slots (upstream TODO in StatepointLowering);
the OxCaml `StatepointStableRootHomes` mechanism handles only
`phi(seed, relocate-of-self)`. Generalizing it to arbitrary GC phis with
per-edge seed stores would remove per-iteration stores + join-block slot
shuffling (boyer tautologyp, kb rporec). Longer-term direction discussed:
derive frametables entirely from RA state (gc-ness bit on vregs,
LiveStacks/VRM as source of truth, in-place root updates) - yesterday's
OxCamlStatepointSpillRoots pass is the seed of that design.

## Latent parser.pp.ml GC miscompile: FIXED (2026-06-10)

## Current Goal

Keep the LLVM backend self-stage2-clean, then improve runtime performance until
the LLVM-built compiler beats both native and the older LLVM baseline on total
microbench, minibench, and compiler benchmark time.

## Latent parser.pp.ml GC miscompile: FIXED (2026-06-10)

See FINDINGS.md (repo root) for the compact full story. Summary:

- Root cause: with the register-preserving alloc calling conventions, greedy
  RA live-range splitting leaves spill-slot copies of GC values whose
  LiveStacks intervals cross statepoints where that value family is not an
  operand. The GC relocates all listed roots; the unlisted crossing slots go
  stale and the merged home store-back block reloads them into RS4GC home
  slots. InlineSpiller statepoint-operand folding only covers slots that are
  the value's active location AT the statepoint. (The "+24 descriptor
  arithmetic" theory was disproven: statepoint FI operands and spill
  instructions are remapped consistently by StackSlotColoring.)
- Fix (metadata-only, zero mutator instructions): new machine pass
  `OxCamlStatepointSpillRoots`
  (vendor/llvm-project/llvm/lib/CodeGen/OxCamlStatepointSpillRoots.cpp),
  between RA and VirtRegRewriter. Collects GC slot families globally
  (folded gc operands + VRM original slots of gc reg operands), appends each
  slot live into-and-across a statepoint as a folded gc operand + gc-map
  pair + FixedStack MMO. Gated on gc "oxcaml"/"ocaml";
  `-oxcaml-statepoint-spill-roots[-verbose]`.
- Validation: -verify-machineinstrs clean; single-module swap repro exit 0
  (was deterministic SIGSEGV); OXCAML_YOUNG_FLIP s=1k/4k clean; fresh
  self-stage1 AND self-stage2 builds pass (stage2 had failed twice on this
  bug); parser.pp.ml s=1k..64k clean on both fresh stage compilers + flip
  on stage2; lit = the 20 known pre-existing failures only;
  SELF_STAGE=2 ocamltest `6756 passed / 284 skipped / 0 failed`
  (`ocamltest_stage2_spillroots_clean_20260610.log`) — the stage2 bar.
- Testsuite fallout fixed along the way (from the earlier committed
  prologue-entry rename to `caml_llvm_call_realloc_stack_stkarg`, not from
  this fix): `_build` runtime needed `make -s runtime-stdlib` for the
  ocamltest fake root, and `stack_check_size_contract.sh`/`challenges.sh`
  now accept the new prologue slow-path symbol.

## Current Change (v3: full call homes, non-volatile slots, init-store pruning)

- Root-caused and fixed the call-statepoint homes miscompile: init stores were
  classified by BLOCK (any entry-block store was treated as initialization),
  so a consumer-NCD defining store that legitimately sits in the entry block
  below interior call statepoints (call statepoints do not split blocks) was
  skipped by `SlotDefStoreDominates`/`oxcamlRootSlotHasSingleDefiningStore`/
  `CanAliasToSlot`.  A channel in `Cmi_format.read_cmi_lazy` was homed at a
  statepoint above its defining store, the reload read the init immediate `1`,
  and SSA repair rewired the defining store to that reload.  Init stores are
  now classified by VALUE: constant operand and entry block
  (`isOxCamlRootSlotInitStore`).  Verified on the isolated cmi_format repro
  (exit 139 -> 0), the exact culprit ordinal (budget=67 skip=66), and an
  829-module object swap of the whole boot compiler.
- `-rs4gc-oxcaml-value-slot-homes` default is now `3` (invokes AND calls).
- Exception-root slot accesses are now NON-volatile by default
  (`-rs4gc-oxcaml-volatile-exnroot-slots`, default false).  Slots escape into
  statepoint gc-live bundles, so alias analysis already forbids forwarding
  across registered statepoints; RS4GC runs in `addIRPasses` before
  CodeGenPrepare/ISel, and non-volatility lets ISel drop dead reloads and
  forward between statepoints.
- Entry-block init stores are pruned when the slot's single defining store
  dominates every load and every statepoint listing the slot
  (`-rs4gc-oxcaml-prune-root-slot-init-stores`, default true).  This removes
  3 dead stores + an immediate materialization from `unify1`-shaped hot
  paths (probes had been paying them on every call).
- Static verifier added: `-rs4gc-oxcaml-verify-root-slots` checks every slot
  reload is dominated by the slot's defining store; clean across all 839
  stashed modules with the final configuration.
- Probes (medians, ratio vs native; layout-luck caveat below): boyer 0.99x
  (was 1.038x committed, 1.052x at head), `catch_failure_then_unify`
  1.256x -> ~1.08-1.11x, `nested_failed_unify` ~1.0-1.2x depending on code
  layout.  NOTE: these tiny probes swing +/-15% from function-alignment
  layout luck alone (verified: same binary +64B alignment moved
  nested_failed_unify 1.398x -> 1.025x); `-align-all-functions` was tested
  and rejected (helps one probe, hurts another).  Only
  `boyer_like_failed_unify` keeps a layout-stable residual (~1.24x).
- Validation of the final configuration: lit at the known pre-existing
  failure sets; cmi_format + 829-module swap repros clean; three fresh boots
  (homes=3, +non-volatile, +pruning) each with the stdlib.pp.ml GC-stress
  sweep s=1k..256k clean, with an always-on young-root checker runtime
  installed during the sweeps; self-stage1 clean
  (`self_stage_v5.log`).
- Stage2 is currently BLOCKED by the pre-existing bug below: the v5 stage1
  compiler hit the latent parser.pp.ml SEGV during the stage2 boot phase in
  two consecutive builds (`self_stage2_v5.log`, `self_stage2_v5_retry.log`),
  while the committed-state stage2-v4 run had passed by luck (its compiler
  crashes on the same repro at s=1k).  Re-running the exact failing command
  standalone passes at default heap params - the in-build trigger is
  layout/timing sensitive.  The full ocamltest suite was therefore run
  against the v5 STAGE1 install instead (`SELF_STAGE=1`):
  `6756 passed`, `284 skipped`, `0 failed`
  (`ocamltest_stage1_v5.log`).
- Final benchmark totals for the v5 configuration (2026-06-10, this machine):
  - Compiler module medians: LLVM/native `0.9732`, `2.75%` speedup
    (`compiler_bench_current_vs_native_20260610_002708.json`).
  - Micro (44 cases): total-time ratio `0.707`, geomean `0.658`, worst case
    `1.13` (`micro_v5_20260610.log`).
  - Minibench: total-time ratio `0.885` (`11.5%` speedup), boyer `1.012x`
    (was ~1.13x), worst case binary_trees `1.117x`
    (`minibench_v5_20260610.log`).

## Known Pre-existing Bug (discovered 2026-06-10, NOT caused by v3)

The stage2-v5 build initially failed: the stage1 compiler segfaulted
compiling `parser.pp.ml`.  Investigation showed a LATENT GC-stress
miscompile that PRE-EXISTS this work entirely:

- Deterministic repro: any LLVM-built stage1/stage2 `ocamlopt` compiling
  `parser.pp.ml` (cwd `_llvm_self_stage2_boot_build/default`) with
  `OCAMLRUNPARAM=s=1k..64k` segfaults 100%; at default heap params it is
  flaky (this is why stage2 builds usually pass and occasionally die).
- Crash: `Ident.compare` receives corrupt arguments (`0` and `1`) via
  `find_value_approximation`'s map lookup; fp-chain:
  `Closure_conversion.cont_96_350+5052` -> `classify_fields_of_block+152`
  -> `List.map` -> `compare`.
- Pre-existence proven: the committed-state stage2-v4 compiler crashes
  identically; so do stage1 builds with EVERY feature combination including
  homes=0/volatile/no-pruning; so does the BASELINE-era clang
  (`llvm-build-clean-head`, 57e9764b3c) and the pre-redesign clang
  (`llvm-build-old-rs4gc`).  The native compiler is clean at s=1k.
  This is very likely the same deep bug previously blamed on call homes via
  the `Lambda_to_flambda.cps` ordinal-304 repro.
- Module isolated: with ONLY `flambda2_from_lambda__Closure_conversion.o`
  LLVM-built (every other object native), the crash reproduces; all-native
  is clean.  Repro loop ~90s: `llstash/test-cc.sh "<extra clang flags>"`
  (stashed IR `/tmp/ccstash/flambda2_from_lambda__Closure_conversion.*.ll`,
  object swap + relink via `llstash/driver5.py <module-list-file>`, good
  tree `_native_build/main`, bad tree `_llvm_self_stage_main_build/main`).
- Ruled out: value-slot homes (crashes at homes=0), slot aliasing, lazy
  boundary loads, volatile vs non-volatile slots, init-store pruning,
  register roots at non-alloc callsites (only at realloc_stack /
  local_realloc / caml_call_gc sites, which save all regs), trap-byte
  offset mismatches in `find_value_approximation` (descriptors audited
  consistent: framesize 64 / offset 40 under active trap vs 48/24 outside),
  missed-young-root scanning (always-on checker runtime silent - though it
  shares the frametable, so it is blind to frametable holes).
- The DEBUG runtime (`-runtime-variant d`) does NOT reproduce (compile
  succeeds at s=1k) - the bug needs the stock runtime's exact allocation
  pattern.
- Remaining suspects: statepoint live-set hole or stack-slot sharing in one
  of `Closure_conversion`'s large functions, or a runtime/frametable scan
  disagreement only visible under precise minor-GC timing.
- The `parser.pp.ml` s=1k repro should be added to the standard validation
  gate once fixed; the stdlib.pp.ml sweep alone does NOT catch it.

### Round 5 (2026-06-10): frametable offset semantics, synth harness

The fix candidates from round 4 were tested and REFUTED for this crash:
SP-relative resolution for statepoint stackmap operands (PreferFP=false -
kept as hardening with a fatal on non-SP resolution, but descriptors were
already SP-resolved), registering all exnroot slots on all statepoints
(cont has NO exnroot slots - no trap regions), and register-coalescer
involvement (-join-liveintervals=0 still crashes).  The corrupting GC
suspends cont at an ALLOC statepoint whose IR carries proper relocates;
the stale value (`simple`) is read after the GC from a frame slot the
descriptor does not cover, flows through add_simple_to_substitute
(closure_conversion.ml:1467) into the substitute map, and cps faults on
it (young-flip, deterministic, cps+396).

NEW INSTRUMENT: llstash/synth-statepoint-offsets*.ll - tiny synthetic
oxcaml modules whose emitted frametables can be diffed against the
actual frame layout by eye, no GC runs needed.  Findings so far:
- The "statepoint-id" attribute ENCODES trap depth: bit0 = alloc,
  ((id>>1)&7)*16 = ActiveTrapBytes which OxCamlGCPrinter ADDS to every
  stack root and to nothing else; a synthetic id of 18/19 silently
  shifts roots by +16.
- AArch64RegisterInfo::eliminateFrameIndex (statepoint case) ALSO adds
  AFI->getOxCamlActiveTrapBytes(MI) (computed from real MIR trap pushes)
  into the operand offset.  RS4GC-converted statepoints (default IDs, no
  trap bits) therefore get the +16-per-trap exactly once (verified
  consistent in find_value_approximation); LLVMIZE-EMITTED statepoints
  with explicit IDs inside trap regions are suspected of DOUBLE-counting
  (ID bits + AFI bytes) - shifting every stack root by +16 per depth.
- The guilty cont statepoint has ID 196609 (alloc, ID-depth 0); its
  emitted stack roots are 192/200 while the MIR-listed slots sit at
  +24 less - the remaining unexplained delta.  Next: replicate cont's
  exact shape in the synth harness (alloc id, several relocated values,
  448-byte frame, split prologue) and diff descriptor vs actual slots;
  each iteration is a pure compile, minutes.

### ROOT CAUSE FOUND (2026-06-10, round 4)

The mechanism is proven with a new debug instrument, OXCAML_YOUNG_FLIP
(runtime/minor_gc.c + domain.c, env-gated): the minor heap alternates
between two locations each collection and the retired space is
PROT_NONE'd, so any dereference of a stale young pointer faults AT THE
GUILTY INSTRUCTION.  Requires s>=4k (16K pages).  With it:

- The crash chain is: an LLVM-compiled statepoint in
  `Closure_conversion.cont` leaves a STALE young pointer in a frame slot;
  cont passes the stale `simple` to `Env.add_simple_to_substitute`
  (closure_conversion.ml:1467); the pair is stored in the substitute map;
  `Lambda_to_flambda.cps` (Lvar case) immediately looks it up and
  dereferences -> fault at cps+396 (or, unprotected, reads recycled
  memory -> the Ident.compare(0,1) crash).
- Guilty statepoint #1 (default config): the alloc statepoint at
  cont+2240 (Ltmp951).  Its descriptor lists the values as REGISTER
  roots (gc_regs) plus slots 192/200 - but the code after it reloads the
  same values from RA SPILL SLOTS 168/176, which are NOT in the
  descriptor.  The GC updates the registers; the spill slots keep the
  pre-GC pointers; the post-GC reloads resurrect them.
- With -max-registers-for-gc-values=0 (zero register roots), the SAME
  fault occurs via a different cont alloc statepoint (cont+1620,
  Ltmp1031): an RA spill slot (~offset 32-48) carrying the value across
  the statepoint is again absent from the descriptor's root list.

UNIFIED ROOT CAUSE: LLVM's register allocator (greedy/InlineSpiller/
split machinery) carries GC values across STATEPOINTs in locations the
statepoint operand list does not reference - sibling registers kept live
because the oxcaml alloc calling convention PRESERVES registers, and RA
spill slots whose live range crosses the statepoint.  The frametable
only describes the operands' locations at the statepoint, so the GC
updates those and every other copy goes stale.  Stock LLVM avoids this
because (a) caller-saved registers die at calls, (b) gc pointers in
callee-saved registers are force-spilled (AllowGCPtrInCSR=false), and
(c) relocate defs end the input interval at the statepoint.  The oxcaml
register-preserving alloc CC re-opens the hole.  Relevant code:
FixupStatepointCallerSaved.cpp + AArch64RegisterInfo::
shouldSpillStatepointGCPtr (only forces x16-x18/x26-x28 today).

FIX DIRECTION (next session): make statepoint-crossing GC live ranges
single-location.  Candidates: (1) make RA statepoint-aware so a GC vreg
live across a statepoint is forced fully into its statepoint-listed
location (fold + no sibling copies); (2) have the statepoint CLOBBER
GC-holding registers from RA's perspective (kill dual residency, accept
spills); (3) at MIR fixup time, enumerate LiveStacks intervals crossing
each statepoint whose original vreg was a GC pointer and append their
slots to the stackmap.  (3) is likely the least invasive and matches the
frametable design.  Verify any fix with OXCAML_YOUNG_FLIP=1 s=4k on the
parser repro, then the stdlib sweep and full stages.

### Second investigation round (2026-06-10, bug still open)

Two REAL adjacent holes were found and fixed (neither is the parser crash):

- RS4GC rematerialization walked GEP chains THROUGH `!is_base_value` GEPs
  (comballoc secondary object starts), so a derived pointer could be
  rebuilt after a statepoint from a DIFFERENT object's relocation.  Both
  chain walkers now stop at marked GEPs (they are the chain root); the
  comballoc-object-starts test now rematerializes fields from their own
  object's relocation.  This path is real but unexercised in
  closure_conversion (objects byte-identical before/after the fix).
- The inline-asm prologue stack checks emitted by `emitOxCamlStackCheck`
  (AArch64FrameLowering.cpp) called the NATIVE `caml_call_realloc_stack`,
  which saves the live x29; stack growth then walks the frame-pointer
  chain from it (fiber.c WITH_FRAME_POINTERS rewrite) into LLVM frames,
  rewriting any spilled word that looks like an old-stack address.  Added
  `caml_llvm_call_realloc_stack_stkarg` (runtime/arm64.S): same stack-arg
  protocol but stores xzr to terminate the chain, exactly like
  `caml_llvm_call_realloc_stack`.  NOTE: runtimes must be rebuilt from the
  new arm64.S before linking code from the new clang (stage0
  `_install/lib/ocaml/libasmrun.a` was patched in place with the new
  arm64.o).
- Also added: `-rs4gc-oxcaml-verify-object-starts` (reports base-pointer
  chains crossing object-start GEPs) and an `OXCAML_LLVM_NO_COMBALLOC`
  env-var gate in asmgen.ml for bisection.

Additional EXCLUSIONS for the parser crash (each tested directly):
comballoc entirely off (module IR regenerated without it - still crashes),
`-O1` (still crashes; not an -O3-only pass), GC values forced to spill
slots (`-max-registers-for-gc-values=0`), the realloc fp-chain rewrite
(disabled in fiber.c - still crashes), ALL stack reallocation (64M-word
initial main stack - still crashes), i64-laundered GC pointers (the
`-rs4gc-heuristic-report-oxcaml-statepoint-crossing-inttoptr` hits all
triage to immediates: or-tag patterns, local_sp arena offsets, Int_ids
table ids).

Repro status: FULLY DETERMINISTIC under lldb (no ASLR): identical crash
pc (`Ident.compare+48`) and registers (x0=0, x1=1) every run, with the
fp-chain compare <- List.map+172 <- classify_fields_of_block+152 <-
curry2 <- cont_96_350+5052.  (Watchpoints perturb signal timing and
change pool-reuse history; use content/condition triggers, not hit
indexes.)

Round-3 narrowing (same day): the crash is `CCenv.find_var` called from
`Lambda_to_flambda.cps` on `Lvar` (lambda_to_flambda.ml:480) walking the
closure_conversion_aux VARIABLES map into a node pointer that lands in
RECLAIMED major-pool memory (recycled small values such as 1, 3, 0x1500).
Refuted by direct experiment: init-placeholder leaks (new debug flag
`-rs4gc-oxcaml-tag-init-placeholders` gives every slot placeholder a
unique odd immediate; the crash value stays plain 1 = Val_long 0),
immediate idents inserted through any aux Env add path (conditional
breakpoints never fire), missing write barriers (caml_modify relocation
count differences are sound MIR tail-merging; all calls survive post-opt
IR), and major-GC starvation (o=10000 still crashes).  Remaining
mechanisms: a frametable liveness hole at a closure_conversion statepoint
(live env/map collected) or relocation/SSA wiring handing code a stale or
wrong pointer.  Next experiment documented in the memory note: walk the
map from its root at the deterministic crash and classify the dead node's
parent (header color/generation, sibling fields) to separate
collected-while-reachable from wrong-value-at-construction.

Watchpoint findings: the corrupt cells' last writers are the GC itself
copying the young original verbatim during promotion
(oldify_one <- caml_scan_stack, then oldify_mopup) - so the corruption
predates promotion and the heap/frametable/GC are clean as writers.
Crash-state decode: List.map was invoked by LLVM-compiled
classify_fields_of_block with `f` = a TAG-0 ordinary block whose field0
happens to be Ident.compare's code pointer - NOT a closure (tag 247);
map blindly entered compare with junk args.  A well-formed-but-WRONG
value therefore flows into the f/list arguments inside cont/classify on
a GC-timing-dependent path: prime suspect is SSA-repair /
exnroot-reload / relocation WIRING selecting the wrong value on a rare
statepoint path.  Next: breakpoint at classify's map callsite on the
crash iteration (no watchpoints) and trace f/list register provenance
back through cont's statepoint reloads against the stashed RS4GC IR.

## Previous Change (v2: consumer-NCD store placement)

- Refined the uniform root design: the slot's single defining store now sits
  at the latest point that dominates every protected invoke of the regions
  sharing the slot (nearest common dominator of the consumers), hoisted out
  of cycles while the value stays available, falling back to the definition
  when the value does not dominate that point.  This fixes a 35% regression
  on `nested_failed_unify`: store-at-def had hoisted argument slot stores to
  the function entry, taxing every call of functions like `unify1` whose
  protected region sits on a conditional path.  Shared slots migrate their
  store upward as later regions join (`EnsureDefStoreCovers`), and
  value-slot homes only apply at statepoints dominated by the store.
- Probes after the change (medians, same machine/run, ratio vs native):
  `nested_failed_unify` 1.252x (head 1.250x), `boyer_like_failed_unify`
  1.251x (head 1.228x), `catch_failure_then_unify` 1.224x (head 1.245x),
  `closure_env_in_try_hit` 1.088x (head 1.089x),
  `many_handler_live_roots_raise` 0.666x (head 0.670x), boyer 1.038x (head
  1.052x).  No bad regressions remain; boyer's win is retained.
- Debug tooling added behind flags: `-rs4gc-oxcaml-call-home-budget/-skip/
  -dump` to bisect call-statepoint homing by ordinal.  Using them, the
  call-homes failure was narrowed to a SINGLE homed call statepoint in
  `Lambda_to_flambda.cps` (skip=304 budget=305 with the llstash harness
  reproduces in ~10s); its IR, frametable record, regalloc slot sharing and
  backedge wiring all audit as consistent, so the residual bug is deeper in
  call-statepoint lowering.  Homes stay invoke-only by default.
- Validation: full RS4GC + AArch64 oxcaml lit suites at the pre-existing
  known-failure sets; fresh boot passes the stdlib.pp.ml GC-stress repro at
  s=1k/4k/64k; self-stage log
  `agent-state/test-suite-29e4cd/self_stage_uniform_roots_v4.log`.

## Previous Change

- RS4GC now classifies each pre-RS4GC addrspace(1) value live over a
  safepoint into exactly two categories: handler-live values get one volatile
  root slot stored once at the value's definition (per-invoke stores remain
  only for PHIs defined on unwind edges), reloaded after each safepoint and at
  recovery entries, with the slot registered on the statepoints; everything
  else uses standard gc.relocate.  This replaces the per-role slot
  materialization that gave boyer's `rewrite_with_lemmas` three duplicate term
  slots re-stored before every protected invoke (now: 2 slots, 1 store each
  per definition, none per invoke).
- Boundary rejoins use lazy per-edge reloads of the single slot; selectors and
  recovery loads alias the underlying slot (single-defining-store slots only),
  so SSA repair does not resurrect relocates or per-invoke stores.
- Fixed a latent null `Info.StatepointToken` read in the old
  `canonicalizeExplicitRootHomesAndFilterLiveSets`; homes are now assigned per
  record from the function-wide value→slot map against the original call.
- Benchmarks (this machine, medians): boyer native `0.0854s`, old-HEAD LLVM
  `0.0933s` (1.093x), new LLVM `0.0869s` (1.018x).  `many_handler_live_roots_raise`
  0.66x vs native.  `boyer_like_failed_unify`/`catch_failure_then_unify`
  (synthetic always-raise loops) are ~4-7% slower than old HEAD because the
  old per-invoke stores fed store-to-load forwarding into the handler reload;
  accepted trade for the normal-path win.
- Lit: all oxcaml RS4GC tests pass incl. new
  `oxcaml-exception-root-single-slot-per-value.ll`; the 13 generic RS4GC and
  7 AArch64 oxcaml failures are pre-existing (identical sets at 57e9764b3c).
- The first self-stage run hit two real bugs, found with a deterministic
  reproducer (`_llvm_boot_*` boot compiler compiling `stdlib.pp.ml` under
  `OCAMLRUNPARAM=s=4k`) and an object-level bisect harness (`../llstash`,
  swaps per-module `.o` between head-clang and new-clang dune trees, re-ars
  archives, relinks, reruns the repro):
  - `Info.ExplicitRootHomes` held raw `Value*`; when a homed value was the
    result of an earlier statepoint-rewritten call, the deferred RAUW left
    the home dangling and the replacement gc.result was never SSA-repaired.
    Fixed with `WeakTrackingVH` handles.
  - Slot aliasing (selector/reload sharing a root slot) is unsound when the
    slot's defining store can re-execute during the alias's lifetime; guarded
    by `CanAliasToSlot` (defining store must dominate the alias and not be
    reachable from it).
  - After those fixes, homing live slot values at CALL statepoints (removing
    them from gc-live in favor of the slot) still corrupts the heap under GC
    pressure even though IR-level SSA repair is provably complete; the
    miscompiled module was `lambda_to_flambda` (`cps` function) and the
    failure is below the IR (call-statepoint lowering of slot-homed frames is
    suspected, possibly the CSR root map).  Homes default to INVOKE
    statepoints only (`-rs4gc-oxcaml-value-slot-homes`, 0=off 1=invokes
    2=calls 3=all); calls keep full relocation.  Investigating `=2` with the
    bisect harness is the open follow-up.
- With invokes-only homes: boyer `1.020x` vs HEAD `1.048x` on the same
  machine/run; the boot compiler passes the `s=4k` GC-stress repro at 4k/16k/
  256k minor heaps (head-clang boots pass it too; the fully-homed build did
  not).
- Self-stage validation of the invokes-only configuration: in progress.

## Previous Change

- Current branch HEAD includes `b6c22b9142` (`Enable comballoc for LLVM
  backend`).
- Default mode is back to normal RS4GC/gc.relocate lowering; the global
  all-volatile-root-slot experiment is not the active path.
- Pending validation fixes:
  - LLVM-backend statmemprof native variants for
    `discard_in_callback.ml` and `stop_start_in_callback.ml` now expect the
    `combined-f33` profile shape, matching LLVM comballoc.
  - `tools/setup-llvm-stage4-ocamltest.sh` now builds a real fake-root
    `otherlibs/systhreads` directory and links generated `threads.h` and
    `st_pthreads.h` into it when present. This fixes self-stage2 ocamltest
    compilation of `tests/lib-systhreads/swapgil.ml`.
- `685d252ac0` was unsafe: the exception-root merge/filtering change let the
  LLVM self-stage compiler segfault while compiling `stdlib/bytes.ml`.
- `9f38c181d9` reverts the unsafe LLVM source/test changes from `685d252ac0`,
  while keeping the useful self-stage script fixes.
- The self-stage scripts now allow explicit `LLVM_EXTRA_FLAGS` when needed and
  preserve clean native/LLVM separation.
- Boyer remains a useful slowdown case. A fresh run showed native median
  `0.08898s`, LLVM median `0.09533s`, ratio `1.0714x`.
- In `rewrite_with_lemmas`, pre-RS4GC has one source value `%2` (`term`) live
  through several handler roles: returned when lemmas are exhausted and reused
  when caught `Unify` retries the loop. RS4GC materializes those roles as
  separate exception-root slots (`%exnroot`, `%exnroot124`, and related PHI
  roots), so the first protected call stores the same `%2`-derived value into
  multiple volatile exnroots. This is a conservative artifact of the current
  handler-boundary materialization, not a distinct source value.

## Evidence

- Full installed-compiler LLVM-backend tests passed after the comballoc test
  fixes:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_current_install_llvm_backend_comballoc_fixed_20260608_153338.log`.
- Self-stage build using `_install` as stage 0 passed:
  log `agent-state/test-suite-29e4cd/self_stage2_comballoc_fixed_20260608_153957.log`.
- Second self-stage build using `_llvm_self_stage_install` as stage 0 passed
  and produced `_llvm_self_stage2_install`:
  log `agent-state/test-suite-29e4cd/self_stage2_second_comballoc_fixed_20260608_154430.log`.
- Full self-stage2 ocamltest rerun passed:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_self_stage2_comballoc_fixed_rerun_20260608_155852.log`.
  The previous full run hit a one-off `tests/lib-threads/signal.ml` native
  output miss; focused `tests/lib-threads` rerun and the full rerun both passed
  that test.
- Rebuilt custom LLVM `opt` and `clang` in `../llvm-build`.
- Focused LLVM tests pass:
  `oxcaml-volatile-root-allocas.ll`,
  `oxcaml-self-base-phi-exception-root.ll`, and
  `oxcaml-statepoint-stable-phi-root-home.ll`.
- Full self-stage2 now passes:
  `6756 passed`, `284 skipped`, `0 failed`,
  log `agent-state/test-suite-29e4cd/ocamltest_self_stage_after_rollback_rerun_20260607_105819.log`.
- Current vs native totals after the rollback:
  - Micro: LLVM/native `0.971072`, `2.98%` speedup,
    log `agent-state/test-suite-29e4cd/micro_after_rollback_20260607_110522.log`.
  - Minibench: LLVM/native `0.927656`, `7.80%` speedup,
    results `agent-state/test-suite-29e4cd/minibench_after_rollback_results.json`.
  - Compiler module medians: LLVM/native `0.965285`, `3.60%` speedup,
    results `agent-state/test-suite-29e4cd/compiler_bench_current_vs_native_20260607_110910.json`.
- This beats the saved old LLVM baseline at `57e9764b3c` on all three totals:
  - Old micro ratio was `0.982986` (`1.73%` speedup).
  - Old minibench ratio was `0.932000` (`7.30%` speedup).
  - Old compiler module-median ratio was `0.981823` (`1.85%` speedup).
- Code-review-revise loop on the net improvement stack:
  - `git diff --check 57e9764b3c..HEAD` passed.
  - Focused LLVM tests above passed again.
  - Reviewed stable root home lowering, statepoint slot allocation, regmask
    call-splitting default, and the inactive volatile-root mode. No source fix
    was needed after the rollback.

## Follow-up Opportunities

- 2026-06-29 sqrt lowering fix:
  - AMD64 LLVM mode now selects non-builtin `sqrt`/`sqrtf` through the existing
    AMD64 SIMD selector, reusing the same native-parity path that lowers to
    `llvm.sqrt.*` in Llvmize.
  - Fresh custom LLVM boot build passed using
    `_llvm_sqrt_boot_context_build/default/boot_ocamlopt.exe` and the project
    RS4GC wrapper: `boot fresh ir: 828`.
  - Focused regression script passed:
    `testsuite/tests/llvm-codegen/amd64_sqrt_intrinsic.sh`.
  - Direct probe showed `llvm.sqrt.f64`/`llvm.sqrt.f32` in IR and
    `vsqrtsd`/`vsqrtss` in assembly, with no sqrt libcall in code.
  - Focused `benchmarksgame_ocaml` `nbody_1` rerun with the fresh compiler:
    native median `0.8371s`, LLVM median `0.8357s`, LLVM/native `0.9983`.
    Captured LLVM nbody IR has two `llvm.sqrt.f64` calls and captured assembly
    emits `vsqrtsd` instead of `sqrt@PLT`.
  - Normal `make -s boot-compiler` remains blocked in this checkout by a
    pre-existing boot-workspace dune sandbox/source-copy failure: rules for
    files such as `parsing/lexer.mll`, `parsing/parser.mly`,
    `middle_end/flambda2/parser/flambda_parser.mly`, and `tools/make_opcodes.mll`
    report missing deps under `_build/default` even though the source files are
    present and tracked. The custom LLVM boot script avoids that path.
- Individual slowdowns are still worth investigating:
  `closure_env_in_try_hit` about `1.29x`,
  `closure_env_in_try_no_raise` about `1.25x`,
  `catch_failure_then_unify` about `1.24x`,
  `boyer_like_failed_unify` about `1.23x`, and minibench `boyer` about `1.13x`.
- Do not resurrect the global all-volatile-root-slot mode as the default without
  fresh evidence; it regressed total micro time in the latest run.
- 2026-06-29 exception slowdown follow-up:
  - `raise_caught_cross_function` is now traced to LLVM `loop-reduce`
    introducing a second loop-carried tagged call-argument IV for the
    invoke-statepoint. AMD64 runtime-entry clobbers then force that IV through
    an extra spill/reload slot around each protected call.
  - Native AMD64, like arm64, marks all normal registers destroyed at raise; it
    does not rely on registers being live into handlers. The better native shape
    recomputes the tagged call argument at the call.
  - The existing OxCaml LSR guards miss exception statepoints because they look
    for call-form `IntrinsicInst`; protected calls are invoke-form
    `@llvm.experimental.gc.statepoint`.
  - Relinked ablation medians for `raise_caught_cross_function`: native
    `0.0740s`, LLVM `0.0849s` (`1.1470x`), recompute-tag `0.0770s`
    (`1.0406x`), global `--disable-lsr` `0.0798s` (`1.0780x`).
  - Detailed notes and artifacts are in
    `agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/report.md` and
    `.../exception/raise_caught_cross_function/`.
- 2026-06-29 LSR invoke-statepoint fix:
  - Updated LLVM LoopStrengthReduce so OxCaml statepoint detection uses
    `CallBase`, covering both call-form and invoke-form statepoints.
  - Made statepoint call-argument LSR skipping the default, turned the broad
    whole-statepoint-loop LSR disable back into an opt-in escape hatch, and
    kept pre-inc loop-exit IVs for OxCaml loops containing statepoints. This
    preserves the original single IV for protected call args instead of
    creating a second loop-carried tagged IV.
  - Added `CodeGen/X86/oxcaml-lsr-statepoint-invoke.ll`.
  - Validation: rebuilt `_build/llvm-tools/bin/llc`; direct FileCheck pipeline
    for the new test passed; saved `raise_caught_cross_function` post-RS4GC IR
    now emits `leaq 1(%rax,%rax), %rax` at the call and no `addq $2`.
  - Quick benchmark reruns with the real wrapper and rebuilt tools:
    `raise_caught_cross_function` `1.0729x` LLVM/native (`SAMPLES=3`) vs the
    earlier corrected broad baseline around `1.1734x`; loop-invariant GC
    dynamic probe `1.4490x` vs earlier `1.5823x`. Full-suite numbers still need
    a longer rerun.
- 2026-06-29 exception slowdown class-2 follow-up after the LSR fix:
  - Fresh focused exception microprobe run with rebuilt local `llc`
    (`SAMPLES=5,WARMUPS=1`, `_install/bin/ocamlopt.opt`, local RS4GC wrapper):
    `raise_caught_cross_function` `1.0884x`, payload cross-function `0.9398x`,
    `closure_call_many_handler_live_roots_raise` `1.1912x`,
    `boyer_like_failed_unify` `1.1314x`, `catch_failure_then_unify` `1.1020x`,
    `nested_failed_unify` `1.0144x`.
  - Native and LLVM both use trap-stack recovery and both clobber ordinary
    registers at raise. Remaining slowdowns are state placement around
    protected calls and, in Boyer/catch, real exnroot/relocate pressure.
  - `closure_call_many_handler_live_roots_raise` is the clean live-root case:
    2 post-RS4GC exnroot allocas, no `gc.relocate`s, hot invoke carries
    `gc-live(ptr %...exnroot)`, and final asm still spills scalar loop state
    through frame slots around the invoke.
  - `catch_failure_then_unify`/Boyer are mixed cases: 8 post-RS4GC allocas,
    14 statepoints, 11 relocates, three exnroot homes in `unify1`, plus folded
    reloads around protected calls.
  - `-rs4gc-oxcaml-exn-ssa-roots` had no effect on the representative AMD64
    artifacts. Forcing `-rs4gc-oxcaml-exn-ssa-all` reduced catch's root slots
    (`alloca` 8 -> 2, exnroot refs 51 -> 10) but made the asm shape worse
    (larger frame and folded reloads 5 -> 7), so it is not a default fix.
  - Current no-spill-fusing ablation is mixed: helps
    `raise_caught_cross_function` (`1.0884x -> 1.0356x`) and Boyer slightly
    (`1.1314x -> 1.1127x`), barely changes
    `closure_call_many_handler_live_roots_raise` (`1.1912x -> 1.1844x`),
    worsens catch (`1.1020x -> 1.1152x`), and badly worsens nested
    (`1.0144x -> 1.2023x`). Global spill-fusing suppression is not the
    exception fix.
  - Updated `amd64-arm-parity-plan.md` with the class-2 findings and next plan:
    keep the existing exnroot/value-home root mechanism, inspect MIR state
    placement around invoke-statepoints, and compare against arm64 artifacts
    before proposing a narrow AMD64 placement/root-materialization change.
  - Followed up with MIR cuts at `greedy`, `oxcaml-statepoint-spill-roots`,
    `virtregrewriter`, and `prologepilog` for
    `closure_call_many_handler_live_roots_raise` and
    `catch_failure_then_unify`; see
    `agent-state/test-suite-29e4cd/slowdown_deep_dive_20260629/exception/class2-mir-findings.md`.
    Result: closure's hot invoke lists only the exnroot home and has ordinary
    scalar/domain-state frame homes; catch's `run_4_9_code` correctly gains
    one backend-listed GC spill root (`%stack.4`) while exception temporaries
    are rejected. The next implementation target is compact native-like
    placement/reload around invoke-statepoints, not frontend roots, global
    no-spill-fusing, or forced SSA exn roots.
- 2026-06-29 compiler-binary benchmark rerun:
  - Compared `_native_install/bin/ocamlopt.opt` against
    `_llvm_current_stage2_install/bin/ocamlopt.opt`; both compilers compiled
    representative compiler modules in normal native mode with no
    `-llvm-backend`.
  - Used matching build trees/CMIs: native side `_native_build/main`, LLVM side
    `_llvm_current_stage2_main_build/main`. Verified `_native_build/log` does
    not contain `llvm-backend=1`.
  - Modules: `cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`,
    `typecore`, and `typemod`; 1 warmup, 7 measured repetitions.
  - Result artifact:
    `agent-state/test-suite-29e4cd/compiler_bench_native_vs_llvmbuilt_native_mode_20260629_023513.json`.
    Sum of module medians: native `16.0902s`, LLVM-built `16.1625s`, ratio
    `1.0045x`. Round-total median: native `16.0943s`, LLVM-built `16.1595s`,
    ratio `1.0041x`. Module-ratio geomean `1.0076x`, median `1.0125x`.
  - Largest slow module ratios: `cfg_selectgen` `1.0293x`, `typecore`
    `1.0141x`, `translcore` `1.0127x`, `typemod` `1.0125x`. `llvmize`
    (`0.9912x`) and `env` (`0.9937x`) were slightly faster with the
    LLVM-built compiler.
- 2026-06-29 BOLT compiler-binary experiment:
  - Built BOLT tools in a separate CMake tree:
    `_build/llvm-bolt/bin/llvm-bolt`, `perf2bolt`, and `merge-fdata`.
  - Relinked the LLVM-built compiler from the existing stage2 build products
    with `-ccopt -Wl,--emit-relocs`, producing
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/ocamlopt.reloc`.
    The relinked binary has `.rela.text`, `.rela.rodata`, `.rela.eh_frame`,
    `.rela.data*`, and `.symtab`.
  - Fixed a local BOLT runtime build issue in
    `vendor/llvm-project/bolt/runtime/instr.cpp`: GCC emitted a mangled
    anonymous-namespace reference to `__bolt_instr_conservative`, while BOLT
    emits the runtime data symbol unmangled. Forcing the asm symbol name let
    `llvm-bolt -instrument` emit an instrumented binary.
  - Full profile-guided BOLT is still not measurable in this environment.
    `perf record` is blocked by `kernel.perf_event_paranoid=4` for hardware and
    software events, direct `sysctl` is denied, and passwordless `sudo` is not
    available. BOLT instrumentation of OCaml-managed code segfaults immediately
    in `caml_garbage_collection`/`caml_do_call_gc`, consistent with inserted
    instrumentation calls/return addresses violating OCaml frame-table/GC stack
    assumptions.
  - A no-profile BOLT rewrite of all code also segfaults on `-version`. A
    minimal rewrite with `-skip-funcs='.*'` runs, confirming the toolchain and
    relocation-enabled binary are basically usable. A conservative runnable
    subset that skips all `caml*` functions also runs:
    `ocamlopt.nocaml.bolt`.
  - Benchmarked the runnable subset against the relocation-enabled base, both
    compiling the same compiler modules in native mode with the same
    `_llvm_current_stage2_main_build/main` CMIs and
    `_llvm_current_stage2_install/lib/ocaml`; result artifact:
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/bolt_skip_caml_vs_reloc_bench_20260629_025323.json`.
    Sum of module medians: base `16.1975s`, BOLT-skip-caml `16.3050s`, ratio
    `1.0066x`. Round-total median: base `16.2409s`, BOLT-skip-caml
    `16.2972s`, ratio `1.0035x`. Geomean module ratio `1.0023x`.
  - Conclusion: the current runnable BOLT subset has no useful win. The
    potentially interesting BOLT measurement requires either enabling perf
    sampling for the uninstrumented compiler or teaching BOLT/OxCaml how to
    rewrite OCaml-managed functions while updating/preserving the runtime frame
    metadata needed by GC and exceptions.
- 2026-06-29 BOLT follow-up with perf enabled:
  - Enabled perf sampling externally and collected LBR data for a corrected
    relocation-enabled compiler. Built/reused BOLT tools:
    `_build/llvm-bolt/bin/llvm-bolt`, `perf2bolt`, and `llvm-bat-dump`.
  - Fixed AMD64 native frametable emission to live in `.data` like the arm
    backend shape: `backend/amd64/emit.ml` now emits the frametable as a data
    object symbol, uses zero fill, and uses data-section label metadata for
    frame-table-relative entries. A first attempt with text-section label
    metadata broke native emission; matching arm's data-section metadata fixed
    that.
  - Rebuilt LLVM self-stage through stage2. Stage2 passed only after reducing
    dune parallelism with `DUNE_BUILD_FLAGS=-j2`; the earlier high-parallelism
    stage2 boot build crashed during large Flambda2 module compilation. The
    resulting `_llvm_boltfix2_stage2_install/bin/ocamlopt.opt` passes a native
    allocation/exception smoke test.
  - Relinked the stage2 compiler with `-ccopt -Wl,--emit-relocs`:
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/ocamlopt.boltfix2.reloc`.
    The startup frametable is in `.data` and has relocations.
  - Added
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/patch_ocaml_frametables.py`.
    BAT alone is too imprecise for OCaml frame descriptors because BOLT shortens
    instructions inside basic blocks. The patcher now disassembles old/new
    binaries and maps old frame descriptor PCs to new call return PCs by call
    target and BAT-estimated original call instruction address. Matching on the
    call instruction, rather than the return PC, is required for reordered
    blocks because return PCs can sit at BAT block boundaries. For split
    functions, the patcher parses BOLT cold-to-hot BAT entries and matches hot
    plus cold fragments together against the parent input function.
  - No-profile BOLT now works for OCaml-managed code:
    `ocamlopt.boltfix2.noprofile.bat.callpatched` starts and compiles/runs the
    native smoke test. The patcher rewrote all 224,538 descriptors by call-site
    mapping with zero unresolved descriptors.
  - Collected a profile from the reduced standalone compiler-module workload
    (`cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`) and converted it
    with `perf2bolt`:
    `ocamlopt.boltfix2.subset.lbr.perf.data` and
    `ocamlopt.boltfix2.subset.lbr.fdata`. `perf2bolt` read 187,793 samples and
    2,999,194 LBR entries.
  - Profile-guided BOLT with function reordering only works:
    `ocamlopt.boltfix2.profile-funconly.bat.callpatched` starts and passes the
    reduced compiler-module workload. Command used `-data=...subset.lbr.fdata`,
    `--enable-bat`, `-reorder-functions=hfsort`, `-reorder-blocks=none`,
    `-align-macro-fusion=none`, and `-peepholes=none`.
  - Full profile-guided BOLT now works on the reduced compiler-module workload:
    `ocamlopt.boltfix2.fullbolt-nosplit.bat.calladdrpatched` passes with
    `-reorder-blocks=ext-tsp` and `-reorder-functions=hfsort`, and
    `ocamlopt.boltfix2.fullbolt.bat.coldpatched` passes with
    `-split-functions -split-strategy=profile2` as well. The successful split
    patch rewrote all 224,538 frame descriptors by call-site mapping with zero
    BAT fallback mappings.
  - Quick timing on the reduced workload, three samples:
    stage2 real median `9.585s`; relocation-enabled median `9.634s`;
    no-profile BOLT median `9.514s` (`0.988x` vs reloc); profile-loaded
    no-reorder median `9.576s` (`0.994x`); profile function-only median
    `9.600s` (`0.996x`). Artifact:
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/boltfix2-stable-benchmark.json`.
  - Full BOLT timing on the same five-module workload with three compile
    repetitions per timing sample: relocation-enabled median `28.849s`;
    no-profile BOLT median `28.344s` (`0.983x`); profile function-only median
    `28.729s` (`0.996x`); full BOLT no-split median `27.807s` (`0.964x`);
    full BOLT split median `28.230s` (`0.979x`). Artifact:
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/boltfix2-fullbolt-benchmark.json`.
  - Next BOLT tasks: promote the patcher into maintained tooling, validate the
    BOLTed compiler on a larger native build/test slice, and then compare the
    BOLTed LLVM-built compiler against the native-built compiler.
- 2026-06-30 BOLT ICP descriptor-synthesis prototype:
  - Added an explicit `--synthesize-icp-descriptors` mode to the local
    frametable patcher. For the shared-return fallback shape currently emitted
    by this BOLT experiment, the default remains fail-closed: recognized
    promoted direct-call return PCs that cannot share the original
    indirect-call shared-return descriptor are reported as unresolved instead
    of silently producing an unsafe executable. This is not yet a complete
    detector for every possible BOLT ICP shape.
  - The synthesis mode duplicates the shared-return frame descriptor, rewrites
    only the descriptor return address, appends a small synthetic frametable at
    the end of the writable `PT_LOAD` segment, extends that segment, and
    installs the new frametable pointer in the spare zero slot after the
    `caml_frametable` terminator. This is still an experiment for measuring
    full BOLT ICP, not maintained production tooling.
  - Tightened the ICP detector after an unsafe first version. The broad
    heuristic synthesized 16 descriptors and passed `-version` plus four
    compiler modules, but `typing/env.ml` failed with `Fatal error: allocation
    failure during minor GC`. Inspection showed false positives such as an
    ordinary direct-call return in `camlTypes__map_rigid_rec_67_301_code`.
    The detector now only treats a promoted direct-call return as needing a
    descriptor when that return PC contains an unconditional jump to the
    shared continuation. The refined artifact synthesized 4 descriptors.
  - Validation for the refined artifact
    `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-sharedret-regonly.synth2.bat.patched`:
    `python3 -m py_compile` on the patcher passed; patching reported
    `patched 224543 frame descriptor return addresses`, `unresolved 0`,
    `call-site mapped 224444; BAT fallback mapped 99`, and `synthesized 4 ICP
    frame descriptors at 0x37185d8`; `-version` passed; a five-module native
    compile replay passed for `backend/cfg_selectgen.ml`,
    `backend/llvm/llvmize.ml`, `lambda/translcore.ml`, `typing/ctype.ml`, and
    `typing/env.ml`.
  - Full ICP was measurable but not a path to the requested +6% LLVM-only win.
    Seven-sample, three-inner-repetition benchmark against the native-built
    compiler gave baseline median `26.857952s`, candidate median
    `27.102657s`, ratio `1.009111x`, improvement `-0.91%`. The earlier safe
    layout-only BOLT artifact on the same comparison remains much better:
    ratio `0.962296x`, improvement `+3.77%`.
  - Per-module three-sample breakdown for the refined full-ICP artifact:
    `cfg_selectgen` `-1.04%`, `llvmize` `+1.46%`, `translcore` `-1.81%`,
    `ctype` `-1.86%`, `env` `-0.03%`. Full ICP hurts or is neutral on most of
    the compiler-module workload, so the next investigation should compare the
    safe BOLT and full-ICP assembly/profile side by side and identify exactly
    which promoted call sites or layout side effects lose the layout-only win.
- 2026-06-30 LLVM-path root/spill-shape follow-up:
  - Reconfirmed that generic optimization-level changes such as `-O4` are not
    acceptable for the compiler-throughput goal, because they would also apply
    to the native-built compiler under a fair setup. The remaining viable
    changes must affect the LLVM-built path only.
  - The current five-module blocker is still `typing/ctype.ml`: the best safe
    BOLT layout artifact is `+3.77%` overall, with per-module results around
    `+3.91%` to `+4.94%` except `ctype`, which is only `+0.87%`.
  - Existing frametable-root analysis shows an LLVM-path-specific root
    inflation problem in the compiler binary: native has about 197k frame
    descriptors / 524k live roots / 415k stack roots, while the LLVM-built
    compiler has about 225k frame descriptors / 774k live roots / 567k stack
    roots. Hot helper examples also inflate: `Reg.find_opt` goes from 18 live
    roots / 14 stack roots to 39 / 39; `Reg.replace` goes from 18 / 13 to
    63 / 60. That matches the `ctype` perf profile being GC/runtime dominated.
  - Tried a real LLVM-built compiler ablation with
    `LLVM_EXTRA_FLAGS='-mllvm -fixup-scs-enable-copy-propagation=false'` in
    separate `_llvm_nocopyprop_*` build/install directories, using
    `tools/build-llvm-stage5-install.sh`, local `_build/llvm-tools`, and
    `DUNE_BUILD_FLAGS=-j2`. Runtime/stdlib completed (`74` fresh LLVM IR
    compilations), but the main compiler build failed after `401` fresh LLVM IR
    compilations with `Fatal error: allocation failure during minor GC` in
    `.ocamlcommon.objs/byte/_unknown_` and SEGVs in
    `otherlibs/dynlink/.dynlink_compilerlibs.objs/native/_unknown_` and
    `.oxcaml_common.objs/native/_unknown_`. This broad copy-propagation
    ablation is rejected as a build-state/correctness failure, not benchmarked.
  - Do not pursue disabling root mechanisms wholesale: old verifier history
    shows `-oxcaml-statepoint-spill-roots=0` and disabling register roots expose
    real stale-root bugs. The performance path is to make the root/spill
    machinery more precise, especially the slot-root cases in
    `OxCamlStatepointSpillRoots`, and then require a full LLVM build before
    benchmarking.
- 2026-06-30 root-counter instrumentation:
  - Added LLVM `STATISTIC` counters under `oxcaml-statepoint-spill-roots` to
    split appended slot roots into GC-family live stack slots, reload-fed
    sibling slots, store-equivalent sibling slots, and value-home slots. The
    aggregate counter label was corrected from "sibling spill slots" to
    "spill slots" because it covers all appended slot-root classes.
  - Rebuilt LLVM `llc` successfully with
    `cmake --build _build/llvm-tools --target llc -- -j2`.
  - Ran a standalone LLVM-backend compile of `typing/ctype.ml` with
    `-mllvm -stats`, local `_build/llvm-tools`, and output under
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/root_stats_20260630/final/`.
    It passed. The new split shows the holdout module is dominated by ordinary
    GC-family live stack slots, not sibling-repair cases: `1618` GC-family
    slot roots, `11` reload-fed sibling slot roots, no reported
    store-equivalent or value-home slot roots, `9` register roots, `7` slot
    initializations, and `1629` total appended slot roots.
  - Next implementation target should therefore be earlier root-liveness or
    stack-slot precision that reduces GC-family stack slots reaching
    statepoints. Optimizing/removing sibling-slot repair would not materially
    move `ctype`.
- 2026-06-30 rejected rooted-only unobserved slot filter:
  - Prototyped `-oxcaml-statepoint-skip-unobserved-gc-slots`, first as a
    broad "no later slot load/statepoint use" filter and then narrowed to only
    skip a slot when the same value is already rooted by the current
    statepoint. The broad version was unsound: it built after a transient
    replay/resume, but the installed compiler segfaulted while compiling
    `backend/llvm/llvmize.ml`; gdb stopped in
    `Flambda2_types__Type_grammar.must_be_singleton`, showing the analysis had
    confused "slot not read later" with "value dead".
  - The rooted-only version rebuilt `llc`, passed a focused `typing/ctype.ml`
    stats compile, and reduced appended roots in that module from the baseline
    `1629` to `985` (`974` GC-family, `11` reload-fed sibling, `9` register
    roots, `644` unobserved GC-family slots skipped). A fresh `_llvm_skipobs3`
    compiler build needed serial resume (`DUNE_BUILD_FLAGS=-j1`) after
    nondeterministic bootstrap-tool SEGVs under `-j2`; the exact failed rules
    replayed cleanly.
  - The `_llvm_skipobs3_install` compiler passed the five-module smoke
    (`cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`) but regressed
    the compiler-throughput benchmark. Seven samples with three inner
    repetitions against `_native_current_install/bin/ocamlopt.opt` gave
    baseline median `27.032365s`, candidate median `28.469930s`, ratio
    `1.053179x`, improvement `-5.32%` in
    `native-current-vs-llvm-skipobs3-inner3.json`.
  - Conclusion: this local frametable-size reduction is not a viable path to
    the +6% LLVM-only compiler goal. The uncommitted code changes were removed
    rather than carried forward.
- 2026-06-30 ordinary-call in-place ablation:
  - Rebuilt `_build/llvm-tools/bin/llc` from clean sources after noticing the
    previous local binary still contained the rejected unobserved-slot
    experiment. The current source tree does not contain that experiment.
  - Collected helper-profile and runtime-GC stats for `typing/ctype.ml`.
    Native-built and LLVM-built execute essentially the same helper workload:
    `modify_total` differs by only `+0.12%`, and initialize/string-helper
    totals are effectively identical. The LLVM-built compiler does, however,
    spend more time in major-GC work: `promoted_words` `+0.50%`,
    `major_words` `+0.46%`, `major_collections` `22 -> 23`, and
    `major_work_done` `366282246 -> 379147801` (`+3.51%`). This supports the
    root-precision hypothesis for the remaining `ctype` gap rather than a
    generic `-O4`-style optimization-level explanation.
  - Clean `ctype.ml` root-listing stats are back to the committed baseline:
    `1629` appended slots, of which `1618` are ordinary GC-family LiveStacks
    slots, `11` reload-fed sibling slots, and `9` register roots. The largest
    appended-slot functions are broad hot typing code, not one rare repair
    case: `unify_row_field` `105`, `unify_row` `63`, `unify3` `59`, `copy`
    `54`, and `instance_prim_locals` `52`.
  - Ablated only ordinary OxCaml call in-place lowering with
    `-mllvm -oxcaml-statepoint-inplace-calls=false`, leaving alloc-family
    statepoints in-place. Focused `ctype.ml` stats changed from
    `1629` appended slots to `200`, while SelectionDAG statepoint pool slots
    increased from `114` to `3098`. A direct assembly-frametable parse showed
    total `ctype` live roots nevertheless dropped from `20675` to `19257`,
    with stack roots dropping from `17625` to `15443`; the emitted object also
    shrank slightly (`1394312` to `1390584` bytes).
  - Built a full `_llvm_noinplacecalls_install` compiler with that flag
    (`74` runtime/stdlib fresh LLVM IR compilations and `1114` main fresh IR
    compilations). The installed compiler passed the five-module smoke
    (`cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`).
  - The full compiler-throughput benchmark rejected the ablation. Seven
    samples with three inner repetitions against the native-built compiler
    gave baseline median `27.027748s`, candidate median `28.639198s`, ratio
    `1.059622x`, improvement `-5.96%` in
    `native-current-vs-llvm-noinplacecalls-inner3.json`.
  - Conclusion: disabling ordinary-call in-place lowering is useful
    diagnostically, but it is not a valid performance direction and would move
    amd64 away from the arm-style model. The remaining path is to keep the
    in-place design and reduce the ordinary GC-family LiveStacks crossings
    without reverting to the old pool-spill mechanism.
- 2026-06-30 rejected generic-O and ctype-weighted BOLT follow-up:
  - Reconfirmed scope: generic optimization-level changes such as `-O4` are
    not valid for the requested compiler-throughput goal, because the same
    setting would also apply to the native-built compiler in a fair comparison.
    Remaining candidates must be LLVM-path-specific: LLVM backend codegen/root
    quality, LLVM-only profile/layout treatment of the LLVM-built compiler, or
    other changes that do not equally improve the native-built baseline.
  - Collected direct `typing/ctype.ml` GC stats with `OCAMLRUNPARAM=v=0x1000`
    for the current native-built compiler and the best safe LLVM+BOLT artifact.
    Allocation stayed essentially identical (`allocated_words` `560541660` vs
    `560551364`), but LLVM+BOLT still did more major-GC work:
    `promoted_words` `60935935 -> 61242980`, `major_words`
    `66212720 -> 66519765`, `major_collections` `22 -> 23`, and
    `major_work_done` `366283673 -> 379152503` (`+3.51%`). This matches the
    earlier un-BOLTed observation and keeps the `ctype` holdout focused on root
    precision / GC scanning, not instruction-selection-only effects.
  - Tested a valid LLVM-path BOLT profile-weighting experiment rather than a
    generic `-O` change. A first attempt to merge the existing best profile
    with the old `ctype7` fdata failed correctly because the former was
    collected from a BOLT-deployed binary while the latter came from the
    non-BOLT relocation binary (`merge-fdata` refuses to mix them). Collected a
    new compatible three-repetition `ctype` LBR profile from
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched`; `perf2bolt`
    mapped it through BAT cleanly (`27726` samples, `442645` LBR entries, zero
    trace-content mismatches).
  - Merged that compatible ctype profile with the current best profile and
    rebuilt the same safe BOLT shape (`cache+` block layout, `hfsort` function
    order, peepholes, rodata-load simplification, BAT). Frametable patching
    succeeded with `224543` descriptor return addresses rewritten, zero
    unresolved, and zero BAT-fallback mappings. The artifact
    `ocamlopt.constfilter.profiled-plus-bolted-ctype3-cache-hfsort-peep-rodata.bat.patched`
    started and passed the five-module smoke (`cfg_selectgen`, `llvmize`,
    `translcore`, `ctype`, `env`).
  - The full seven-sample / three-inner-repetition compiler-throughput
    benchmark rejected the moderate ctype-weighted profile. Native median was
    `27.044386s`; candidate median was `26.184857s`; ratio `0.968218`;
    improvement `+3.18%`. Artifact:
    `native-current-vs-llvm-constfilter-profiled-plus-bolted-ctype3-cache-hfsort-peep-rodata-inner3.json`.
    This is worse than the current best safe BOLT result (`+3.77%`), so BOLT
    profile weighting is not currently the path to the required `+6%`.
  - Current conclusion: the valid LLVM-path work should move back to backend
    root precision while preserving the arm-style in-place design. The best
    measured artifact remains
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at `+3.77%`;
    the remaining gap is dominated by `ctype`'s extra major-GC/root-scanning
    work.
- 2026-06-30 rejected `mcpu=native` + full BOLT route:
  - Reconfirmed the scope after review: `-O4` and other generic optimization
    level changes are not valid candidates for the compiler-throughput goal,
    because the native-built compiler could receive the same treatment. The
    tested route here is LLVM-path-specific: an LLVM-built `mcpu=native`
    compiler binary relinked with relocations, then optimized with BOLT.
  - Relinked the existing `_llvm_mcpu_native_build/main` compiler objects with
    only `-ccopt -Wl,--emit-relocs` added, producing
    `bolt_compiler_20260629/ocamlopt.mcpu-native.reloc`. The binary has
    `.rela.text`, `.rela.rodata`, `.rela.eh_frame`, data relocations, and
    `.symtab`. It starts, reports config, and compiles/runs the native fib
    smoke with `_llvm_mcpu_native_install/lib/ocaml`.
  - Collected a five-repetition seven-module LBR profile from the relinked
    binary using the matching `_llvm_mcpu_native_build/main` and
    `_llvm_mcpu_native_install/lib/ocaml` context. `perf2bolt` matched the
    build-id, read `288862` samples and `4617718` LBR entries, ignored `3.5%`
    of samples, had zero trace-content mismatches, and wrote
    `ocamlopt.mcpu-native.reloc.compiler7.lbr.fdata`.
  - Ran the same safe BOLT shape as the current best compiler artifact:
    `--enable-bat -lite -reorder-blocks=cache+
    -reorder-functions=hfsort -peepholes=all -simplify-rodata-loads`. BOLT
    produced
    `ocamlopt.mcpu-native.cache-hfsort-peep-rodata.bat.bolt`; frametable
    patching from the clean committed patcher produced
    `ocamlopt.mcpu-native.cache-hfsort-peep-rodata.bat.patched` with
    `224274` descriptor return addresses patched, zero unresolved, all via
    call-site mapping. Startup/config/fib smoke passed, and a one-sample
    workload smoke versus the unbolted `mcpu=native` install was faster
    (`9.713s` -> `9.283s`).
  - The robust native-built comparison rejected the candidate. Seven samples
    with three inner repetitions gave native-built median `26.984741s`,
    candidate median `28.478105s`, ratio `1.055341`, improvement `-5.53%` in
    `native-current-vs-llvm-mcpu-native-cache-hfsort-peep-rodata-inner3.json`.
  - A sanity rerun of the unbolted `mcpu=native` install under the same
    three-inner setup showed that the starting point is already bad here:
    five samples gave native-built median `27.367092s`, `mcpu=native` median
    `29.010651s`, ratio `1.060056`, improvement `-6.01%` in
    `native-current-vs-llvm-mcpu-native-install-inner3-rerun.json`.
  - Conclusion: relocation-enabled full BOLT now works mechanically for the
    `mcpu=native` LLVM-built compiler, but this route is not a path to the
    requested `+6%` over native-built. The best valid artifact remains
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at `+3.77%`.
    The next useful work remains LLVM backend/root precision, especially the
    `ctype` extra major-GC/root-scanning gap, not generic optimization level
    changes or `mcpu=native`.
- 2026-06-30 follow-up after review correction:
  - Rejected `-O4` again as an invalid route. It is not an LLVM-path
    improvement because the native-built compiler could also be run with the
    same driver optimization level; do not use it as evidence toward the
    `+6%` goal.
  - Rechecked frame pointers as a possible LLVM-only explanation. The
    native-built baseline reports `with_frame_pointers: true` and both native
    and LLVM compiler configs carry `-fno-omit-frame-pointer`, so frame-pointer
    removal is not a valid explanation for the LLVM-vs-native compiler gap in
    the current setup.
  - Parsed the existing `typing/ctype.ml` current-vs-no-inplace assembly
    artifacts. The no-inplace ablation reduces the actual frametable live-root
    count by about the same amount as the `OxCamlStatepointSpillRoots`
    appended-slot delta, so the `ctype` GC scanning gap is not just a stats
    artifact. The largest current-minus-no-inplace deltas are in hot
    `ctype` functions: `unify_row_field`, `unify_row`,
    `instance_prim_locals`, `build_subtype`, `copy`, `loop_386`, and
    `unify3`. The shape is mostly stack-root growth with fewer register roots.
    The no-inplace ablation remains rejected because it abandons the arm-style
    in-place ordinary-call design and regressed the full compiler benchmark.
  - Re-read the AMD64/AArch64 OxCaml calling-convention definitions. AMD64's
    ordinary OxCaml calls mostly match the intended arm model:
    `OxCaml_WithoutFP` preserves no GPRs, `OxCaml_WithFP` preserves only
    `RBP`, and the X86 target hook force-spills `RBP` roots because ordinary
    calls do not populate `gc_regs`. The remaining gap therefore looks more
    like AMD64 register-pressure / statepoint operand placement than a
    fundamentally wrong ordinary-call preserved mask.
  - Ran the linked-binary frametable analyzer on the native-built baseline and
    current best BOLT artifact. Native has `197298` descriptors / `523867`
    live roots / `415445` stack roots. The BOLTed LLVM-built compiler has
    `224541` descriptors / `773566` live roots / `566595` stack roots, with
    duplicate roots negligible (`54`). The LLVM-built binary also has a large
    LLVM-specific stack-check target class:
    `caml_llvm_call_realloc_stack` accounts for about `30987` descriptors and
    `73650` live roots. These checks are cold dynamically, but their
    statepoint operand lists and live ranges can still affect AMD64 code
    quality; this is a valid LLVM-path investigation target.
  - Current direction: keep `-O4` and other generic flags out of the goal.
    Continue with LLVM-only fixes: either exact OCaml-aware BOLT frametable
    support for transformations such as ICP, or a backend/root-placement fix
    that keeps the in-place arm-style mechanism while reducing AMD64
    duplicate live homes around ordinary calls and stack checks.
  - Implemented and tested an AMD64-only ordinary stack-check cleanup in
    `backend/llvm/llvmize.ml`: ordinary AMD64 stack checks now call
    `caml_llvm_call_realloc_stack*` as a GC-leaf `Oxcaml_alloc` call instead
    of a statepoint. AArch64 keeps the existing statepoint stack-check path.
    The local correctness review checked that
    `ordinary_trap_unwind_for_basic_safepoint` gives `Stack_check` no ordinary
    unwind edge and that the AMD64 runtime helper saves all OCaml registers
    around `caml_try_realloc_stack`, matching the native AMD64 stack-check
    slow path. Updated
    `testsuite/tests/llvm-codegen/stack_check_size_contract.sh` so AMD64
    expects ordinary stack-check calls to be `gc-leaf-function` and not carry
    a `statepoint-id`, while non-AMD64 hosts keep the old statepoint contract.
  - Validation:
    - Boot LLVM build with `tools/build-llvm-boot-with-installed.sh` passed
      the smoke (`55`).
    - Focused stack-check contract script passed in normal and
      `no-cfg-stack-checks` modes with the boot compiler.
    - Self-stage install built successfully with
      `tools/build-llvm-stage5-install.sh`; the self-built compiler passed the
      native fib smoke.
    - Frametable analysis of the self-built compiler confirmed the
      `caml_llvm_call_realloc_stack` bucket disappeared. The previous best
      BOLT artifact had about `30987` descriptors / `73650` live roots in that
      bucket; the stack-check-leaf self-built compiler and BOLT artifact have
      no such bucket.
  - BOLT relink note: the first relocation-enabled relink accidentally used
    the boot/native stdlib and produced BOLT artifacts that patched with `92`
    BAT fallback frame mappings and crashed in `caml_garbage_collection`.
    Relinking with the matching `_llvm_stackcheck_leaf_install/lib/ocaml`
    stdlib fixed this: no-profile and profiled BOLT both patched with zero BAT
    fallback mappings and passed the fib smoke.
  - Performance result: the un-BOLTed self-built stack-check-leaf compiler was
    still slower than native-built (`-2.63%`, artifact
    `native-current-vs-stackcheck-leaf-install-samples5-inner3.json`). The
    valid full-BOLT stack-check-leaf artifact
    `ocamlopt.stackcheck-leaf.stage-stdlib.cache-hfsort-peep-rodata.bat.patched`
    reached only `+1.81%` vs native-built on the five-module native-mode
    compiler workload (`native-current-vs-stackcheck-leaf-stage-stdlib-cache-hfsort-peep-rodata-samples5-inner3.json`),
    below both the current best valid BOLT artifact (`+3.77%`) and the `+6%`
    goal. Conclusion: removing ordinary stack-check statepoints is a real
    metadata/root-table cleanup and likely worth keeping for stack-check
    quality, but it is not the main throughput lever. The next target remains
    allocation/ordinary-call root precision, especially the `ctype` major-GC
    root-scanning gap.
- 2026-06-30 BOLT ICP descriptor refinement after the `-O4` correction:
  - Reconfirmed that generic `-O4` is out of scope because native-built can use
    the same driver-level optimization; only LLVM-path improvements count.
  - Investigated normal BOLT ICP again on
    `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-calls-v4.bat.bolt`.
    The missing-frame startup site `0x3bb5dc3` is a normal ICP shape:
    BOLT emits a promoted direct `call target` returning to the shared
    continuation, while the fallback `call *reg` returns to a small block that
    jumps to that continuation. There is no `push $return; jmp *callee` in
    this shape, so the current patcher does not rediscover these descriptors.
  - Tried local patcher variants, then restored the patcher source. Variant A
    synthesized descriptors by cloning the exact old return-PC descriptor:
    it patched with zero unresolved descriptors but segfaulted compiling
    `backend/cfg_selectgen.ml`. Variant B synthesized descriptors by cloning
    the output fallback-call descriptor: it synthesized `178` descriptors,
    skipped `4` descriptor-less source returns, patched with zero unresolved
    descriptors, and still segfaulted compiling `backend/cfg_selectgen.ml`.
  - Conclusion: making normal BOLT ICP safe is not just a missing-return-PC
    enumeration problem. The cloned descriptor root maps are not reliable for
    the promoted direct-call state. Do not benchmark normal ICP or count it
    toward the goal until we can prove the exact frame/root state at the
    promoted return PC. The viable paths remain (1) backend/root precision in
    the LLVM codegen path, especially the `ctype` GC-scanning gap, or (2) a
    deeper OCaml-aware BOLT integration that can derive correct descriptors
    for BOLT-created callsites rather than cloning nearby ones.
- 2026-06-30 `ctype` GC-family root split by statepoint kind:
  - Added diagnostic `STATISTIC` counters in
    `OxCamlStatepointSpillRoots.cpp` to split GC-family spill-slot roots by
    statepoint kind. This is codegen-neutral and only affects `-mllvm -stats`
    output.
  - Rebuilt vendored `llc` with `cmake --build _build/llvm-tools --target llc
    -- -j2` and reran the focused `typing/ctype.ml` IR through
    `tools/llvm-rs4gc-llc-wrapper.sh` with `PATH` forced to
    `_build/llvm-tools/bin`.
  - Result in
    `root_stats_20260630/gc-family-by-cc/stderr.log`: of `1618` GC-family
    spill slots appended in `ctype`, `757` are at alloc-family statepoints,
    `860` are at ordinary-call statepoints, `1` is at a C-call statepoint, and
    none are in the "other" bucket. The remaining root/scanning gap is
    therefore not just ordinary calls; AMD64 alloc-family in-place statepoints
    are also producing many extra live stack homes.
  - Next backend target: inspect hot `ctype` functions around alloc-family
    statepoints and compare AMD64 against the arm-style intent. The likely
    issue is not the presence of the post-RA sibling-root mechanism itself,
    but AMD64 register-pressure / spill-slot placement causing many values to
    have both register roots and live spill homes at alloc and ordinary
    statepoints.
- 2026-06-30 LLVM-path-only follow-up after rejecting `-O4`:
  - Reconfirmed the goal constraint: `-O4` and other generic driver flags are
    not valid evidence for this task because they would also apply to the
    native-built compiler. Candidate wins must come from the LLVM backend,
    LLVM pass configuration, or LLVM-built-binary-only BOLT/profile work.
  - Tested a narrow root-precision idea locally, then reverted it: separate
    "valid OCaml value" from "needs relocation" and skip slots whose unique
    reaching store is provably a tagged immediate or static OCaml data block.
    Vendored `llc` rebuilt, and the focused `typing/ctype.ml` stats compile
    passed, but the new skip count was zero and all root counters remained
    unchanged (`1629` total appended spill slots, `1618` GC-family slots).
    Conclusion: this is not the `ctype` root-scanning lever; do not carry the
    added complexity.
  - Generated `unify_row_field` MIR stopped immediately before and after
    `OxCamlStatepointSpillRoots`. At the hot `caml_apply2` statepoint, the
    pre-OXSR statepoint already lists `14` GC operands. OXSR grows it to `20`
    by adding six live stack homes (`%stack.1`, `%stack.2`, `%stack.3`,
    `%stack.16`, `%stack.28`, `%stack.32`) that would otherwise stale after a
    moving GC. This confirms OXSR is part of the root-count growth, but it is
    repairing real duplicate homes rather than inventing arbitrary roots.
  - Checked a suspected AMD64 calling-convention mismatch. The native backend
    comments mention `rax` through `r13`, but the actual
    `ocaml_int_registers` list is `RAX RBX RDI RSI RDX RCX R8 R9 R12 R13`;
    `R10`/`R11` are intentionally excluded because PLT stubs may clobber them.
    The LLVM AMD64 ordinary OxCaml calling convention uses the same value
    register list, so there is no `R10`/`R11` parity fix to make here.
- 2026-06-30 LLVM-path-only BOLT ICP root-safety screen:
  - Added a benchmark-local analyzer
    `bolt_compiler_20260629/analyze_icp_root_safety.py` to classify BOLT ICP
    promoted direct-call source sites by the original OCaml frame descriptor's
    live-root count. This is not compiler-path code; it correlates BOLT BAT,
    post-ICP `call; jmp merge` shapes, frametable descriptors, and optional
    `.fdata` call counts.
  - Ran it on the unsafe normal ICP artifact
    `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-calls-v4.bat.bolt`
    against `ocamlopt.constfilter.reloc` and
    `ocamlopt.constfilter.reloc.noassert.lbr.fdata`. Result:
    `538` approximated promoted source sites; `491` had nonzero live roots,
    `41` had zero live roots, and `6` had no source descriptor. Weighted by
    `.fdata` call counts, nonzero-live sites accounted for `4396` counts while
    zero-live sites accounted for only `23`.
  - Conclusion: a conservative zero-live-only BOLT ICP filter would be
    correct-looking but too small to plausibly recover the missing throughput.
    The hot ICP opportunity is dominated by nonzero-root OCaml callsites, so
    full ICP needs proper OCaml frame/root metadata for BOLT-created promoted
    return PCs rather than descriptor cloning or a zero-root subset.
- 2026-06-30 rejected scoped allocator partition screen:
  - Tried a narrower version of the earlier `-split-spill-mode=default`
    experiment: in `RegAllocGreedy`, use `SM_Partition` only for OxCaml live
    ranges that cross call regmasks, leaving non-OxCaml and non-call-crossing
    ranges on the normal `SM_Speed` path. This is LLVM-path-only and was meant
    to reduce overlapping complement intervals that become duplicate
    statepoint-crossing stack homes.
  - Focused `typing/ctype.ml` RS4GC stats matched the earlier split-default
    shape: total appended spill slots `4675 -> 4276`, GC-family slots
    `4665 -> 4265`, ordinary-call slots `3335 -> 2703`, but alloc-family
    slots worsened `1301 -> 1533`; assembly lines fell `251284 -> 249951`.
  - Rebuilt vendored `llc` and ran
    `tools/build-llvm-boot-with-installed.sh` with
    `LLVM_WRAPPER=agent-state/test-suite-29e4cd/llc-wrapper.sh`. The boot build
    failed in `middle_end/flambda2/simplify/.flambda2_simplify.objs/native/_unknown_`
    and `.ocamloptcomp.objs/native/_unknown_` with SEGV / minor-GC allocation
    failure, the same failure class as global split-default. Reverted the
    allocator edit and rebuilt `llc` back to the checked-in source state.
  - Conclusion: partitioning call-crossing live ranges does reduce some
    ordinary-call duplicate homes, but it changes the allocator enough to
    miscompile the compiler. Do not pursue this as a direct policy change; any
    allocator-side fix needs a more precise invariant and a small failing
    reducer before performance testing.
- 2026-06-30 `ctype` bottleneck restated after excluding `-O4`:
  - Rechecked existing `typing/ctype.ml` perf reports. Both native and the
    current best LLVM+BOLT artifact are dominated by GC work
    (`do_some_marking`, `oldify_one`, `pool_sweep`, `oldify_mopup`), so BOLT
    code layout has little headroom on this module by itself.
  - Existing GC stats explain why `ctype` barely improves: native has
    `60935609` promoted words, `22` major collections, and `366282246`
    `major_work_done`; LLVM/BOLT has `61242865` promoted words, `23` major
    collections, and `379148840` `major_work_done`, with essentially the same
    total allocation. The gap is therefore extra GC retention/scanning work,
    not ordinary generated-code speed alone.
  - Ran `analyze_frametable_roots.py` over native, un-BOLTed LLVM, and the
    current best LLVM+BOLT artifact. Native has `197298` frames and `523867`
    live roots (`108422` register, `415445` stack); LLVM/BOLT has `224541`
    frames and `773566` live roots (`206971` register, `566595` stack). BOLT
    preserves the LLVM frametable shape, so this is an LLVM-backend root
    metadata issue, not a BOLT layout issue.
  - The old best artifact still shows the pre-stackcheck-leaf
    `caml_llvm_call_realloc_stack` bucket (`73650` roots), but the later
    stackcheck-leaf build removed that bucket and still only reached `+1.81%`.
    The remaining high-value target is therefore alloc-family and ordinary-call
    root precision, especially reducing allocator-created duplicate stack homes
    without weakening the in-place GC mechanism or changing general LLVM flags
    that native-built could also use.
- 2026-06-30 corrected LLVM-path scope after review:
  - `-O4` and other generic driver-level optimization changes are invalid for
    the `+6%` compiler-throughput goal because the native-built compiler could
    use the same setting. Valid candidates are LLVM-backend changes, LLVM
    pass/codegen configuration that only affects the LLVM-built artifact, and
    BOLT/profile work on the LLVM-built artifact.
  - Rechecked the temporary all-path BOLT shared-return prototype and restored
    the patcher/source state. The direct `push $return; jmp target` ICP shape
    duplicates the already rejected `icp-sharedret-v3` result: it starts and
    passes small smoke, but benchmarks at `-4.07%`, so it must not be carried
    forward as a performance fix. The local no-assert `llvm-bolt` binary was
    rebuilt after the source was restored so future BOLT runs do not use stale
    behavior.
  - Rechecked existing safe BOLT results under the corrected scope. The best
    robust whole-workload artifact remains
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at `+3.77%`
    on the seven-sample/three-inner-repetition five-module workload. Other
    safe BOLT screens around `cache-hfsort`, `hfsort+`, tail duplication,
    ctype-weighted profiles, `mcpu=native`, and full-ICP variants either
    rerun lower than best, fail correctness, or regress. BOLT layout alone is
    not currently enough to reach `+6%`.
  - Rechecked AMD64 ABI parity before proposing root changes. LLVM AMD64 uses
    the same ten ordinary OCaml value registers as native
    (`RAX RBX RDI RSI RDX RCX R8 R9 R12 R13`), excludes PLT-unsafe
    `R10/R11`, models ordinary OxCaml calls as preserving no value registers
    (plus `RBP` only under frame pointers), and models alloc-family calls as
    preserving all non-runtime registers except `R10/R11`, matching native's
    `destroyed_at_alloc_or_poll` intent. The remaining root inflation is
    therefore not an obvious calling-convention mismatch.
  - Post-stackcheck frametable comparison: native has `197298` descriptors and
    `523867` live roots; the stackcheck-leaf BOLT artifact has `193324`
    descriptors and `699633` live roots. The old `caml_llvm_call_realloc_stack`
    bucket is gone, but ordinary/debug and alloc-family roots remain much
    larger than native. This keeps the next valid implementation target on
    AMD64 LLVM root/spill precision, not frontend roots, not generic
    optimization levels, and not another ad hoc BOLT call-shape workaround.
- 2026-06-30 rejected normal direct-ICP descriptor cloning retry:
  - Repatched the existing normal direct-ICP BOLT output
    `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-calls-v4.bat.bolt`
    with the current patcher and no synthesis. It reported zero unresolved
    descriptors but immediately aborted on `-version` with
    `caml_scan_stack: missing frame descriptor retaddr=0x3bb5dc3`.
    Disassembly showed the missing PC is the promoted direct call in
    `camlStdlib__List__concat_map_64_160_code`: BOLT emits
    `call caml_tuplify2` returning directly to the shared continuation, while
    the fallback `call *%rcx` returns to a tiny `jmp` to that continuation.
  - Tried a guarded local patcher edit, then removed it. The edit detected
    both normal ICP shapes: promoted direct returns that jump to the fallback
    continuation, and promoted direct returns that are themselves the fallback
    continuation target. It synthesized `220` descriptors from fallback
    descriptors and skipped `5` descriptor-less fallback sites. The resulting
    artifact `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-calls-v4.normalicp2.bat.patched`
    passed `-version`, `-config`, and a tiny native compile.
  - The same artifact failed the five-module compiler smoke while compiling
    `backend/llvm/llvmize.ml` with `Fatal error: allocation failure during
    minor GC`. This is the same wrong-root/corruption class as the earlier
    `synth4` descriptor-cloning attempts, not a missing-descriptor startup
    issue. Conclusion: normal BOLT direct-call ICP still needs an exact
    OCaml-aware root map for the promoted call return PC; cloning the fallback
    descriptor, even with a tighter detector, is not correct enough to carry
    or benchmark.
- 2026-06-30 LLVM-only scope and ICP rerun after review:
  - Reconfirmed the review constraint: `-O4` and other generic driver-level
    optimization changes do not count toward the `+6%` goal because the
    native-built compiler can use them too. Only LLVM-backend/codegen changes,
    LLVM-path pass configuration, or LLVM-built-binary-only BOLT/profile work
    are valid evidence.
  - Ran a read-only stricter normal-ICP detector using BAT proximity to pair
    promoted direct calls with their fallback indirect callsites. It did not
    reduce the candidate set: loose and strict detection both found `220`
    synthesized-descriptor candidates, and the startup missing-descriptor site
    `0x3bb5dc3` remained included. The previous `normalicp2` failure is
    therefore not explained by a simple out-of-neighborhood false match.
  - Reran existing half-skip normal-ICP artifacts to check whether descriptor
    cloning failures localize to a small subset. `synth4-skip156-312` still
    aborts immediately on `-version` with missing descriptor `0x3bb5dc3`.
    `synth4-skip0-156` passes `-version` but aborts during the first
    benchmark module, `backend/cfg_selectgen.ml`. `synth4-skipapply` also
    passes `-version` and aborts on `backend/cfg_selectgen.ml`. This reinforces
    that cloned descriptors are broadly unsafe for BOLT-created ICP return PCs;
    do not count or benchmark these artifacts.
  - Current best valid LLVM-only result remains the safe BOLT layout artifact
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at `+3.77%`
    over the native-built compiler. To get to `+6%`, the next implementation
    work should avoid generic `-O` changes and either add exact OCaml-aware
    BOLT metadata for new ICP return PCs or improve AMD64 LLVM root/spill
    precision while preserving the arm-style in-place GC model.
  - Checked descriptor contents for representative normal-ICP sites from the
    detector output, including the startup `concat_map` site and candidates in
    `caml_apply2`, `caml_apply5`, `camlCfg_dataflow__strong_connect`, and
    `camlX86_gas__print_reg`. The cloned source descriptors sampled there were
    stack-root-only, not register-root descriptors. This rules out a narrow
    "drop fallback-only register roots" fix for the observed corruption. The
    direct-ICP route still needs exact metadata for the direct-call state, not
    a small register-filter heuristic.
- 2026-06-30 instrumented-BOLT BAT screen:
  - Revisited full/instrumented BOLT as an LLVM-built-binary-only route, still
    excluding generic driver optimizations such as `-O4`.
  - Confirmed the local instrumentation runtime symbol fix in
    `vendor/llvm-project/bolt/runtime/instr.cpp` remains relevant: the old log
    failed to link the anonymous-namespace
    `_ZN12_GLOBAL__N_125__bolt_instr_conservativeE` symbol, while the later log
    links the instrumentation runtime successfully.
  - Tried fresh full instrumentation of `ocamlopt.constfilter.reloc` with
    `--enable-bat`, so the existing OCaml frametable patcher could translate
    descriptor return PCs before running the binary. BOLT did not reach output:
    `constfilter-instrumented-bat.log` ends with glibc heap corruption
    (`corrupted double-linked list (not small)`).
  - Tried profile-guided hot-only instrumentation with the existing LBR fdata
    and `--enable-bat`. This also failed inside BOLT before output; the crash
    stack is in `BinaryEmitter::emitFunction` via
    `MCPlusBuilder::getOffset`, logged in
    `constfilter-instrumented-hot-bat.log`.
  - Control run: the same hot-only instrumentation without `--enable-bat`
    succeeded and produced
    `ocamlopt.constfilter.instrumented-hot-nobat`. That unpatched binary does
    not start, which is expected because BOLT has rewritten code addresses
    without emitting BAT data for the OCaml frametable patcher.
  - Conclusion: instrumented BOLT is not currently blocked on the runtime
    symbol fix; it is blocked on making BOLT instrumentation emit usable BAT
    maps (or otherwise exposing an address map) so OCaml frametables can be
    patched before profiling. This remains a plausible LLVM-only path, but it
    requires a BOLT-side fix before any performance measurement can count.
- 2026-06-30 instrumented-BOLT BAT fix and measurement:
  - Fixed the BOLT `--instrument --enable-bat` emitter crash by making
    `MCPlusBuilder::getAnnotationInst` return null when the last nested
    `MCInst` operand is not an `ANNOTATION_LABEL`, instead of relying on a
    debug-only assert and then reading non-annotation operands as annotation
    immediates in no-assert builds.
  - Rebuilt `llvm-bolt`; hot-only instrumentation of
    `ocamlopt.constfilter.reloc` with `--enable-bat` now completes, links the
    instrumentation runtime, and writes `61331` BAT maps. The earlier runtime
    symbol fix for `__bolt_instr_conservative` is still required.
  - Extended `patch_ocaml_frametables.py` to map return PCs for inserted calls
    to `__bolt_instr_ind_call_handler_func` back to the original indirect
    call's frame descriptor. Without this, the patched instrumented compiler
    failed immediately with a missing descriptor at the return after the BOLT
    instrumentation handler call in `camlStdlib__List__concat_map_64_160_code`.
  - Validation:
    - `python3 -m py_compile patch_ocaml_frametables.py` passes.
    - The hot-only instrumented/BAT compiler patches with `224543` descriptors,
      `0` unresolved, and all descriptors call-site mapped after the handler
      return fix.
    - The patched instrumented compiler passes `-version`, a tiny compile, and
      collects an `8.8M` instrumentation fdata while compiling the standard
      five compiler benchmark modules three times.
  - Reapplied the safe `cache+`/`hfsort+`/peephole/rodata BOLT recipe to
    `ocamlopt.constfilter.reloc` using that instrumentation fdata. The final
    binary `ocamlopt.constfilter.instrprof-cache-hfsort-peep-rodata.bat.patched`
    patches with `224543` descriptors, `0` unresolved, and all descriptors
    call-site mapped. It passes `-version` and a one-inner-repetition
    five-module compile smoke.
  - Benchmark result against the native-built compiler, both compiling in
    normal native mode with `samples=7`, `inner_repetitions=3`:
    native median `26.990598s`, candidate median `26.266300s`, ratio
    `0.973164811`, improvement `+2.68%`
    (`native-current-vs-llvm-constfilter-instrprof-cache-hfsort-peep-rodata-inner3.json`).
    This is a valid LLVM-built-binary-only improvement path but does not meet
    the `+6%` target and is worse than the prior best safe BOLT result
    (`+3.77%`). Do not count it as the goal artifact.
  - Also tested full BOLT ICP with the same instrumentation fdata plus
    `--indirect-call-promotion=all --x86-oxcaml-icp-shared-return`. BOLT
    optimized `66.3%` of profiled indirect callsites. The patched artifact
    `ocamlopt.constfilter.instrprof-cache-hfsort-peep-rodata-icp.bat.patched`
    passes `-version` and the one-inner-repetition five-module compile smoke,
    which is a correctness improvement over earlier ICP attempts.
  - Performance is negative: native median `26.968339s`, ICP candidate median
    `27.286278s`, ratio `1.011789326`, improvement `-1.18%`
    (`native-current-vs-llvm-constfilter-instrprof-cache-hfsort-peep-rodata-icp-inner3.json`).
    Therefore the straightforward full-BOLT ICP configuration is not a path to
    the required `+6%`, despite now being benchmarkable.
- 2026-06-30 rejected ordinary-register-root canonicalization:
  - Tested an LLVM-only `OxCamlStatepointSpillRoots` prototype that rewrote
    ordinary managed-call register root operands to the virtual register's
    allocator spill slot when that slot was already going to be appended as a
    GC-family stack root. Focused `typing/ctype.ml` stats looked promising:
    `fixup-statepoint-caller-saved` dropped from `407` allocated spill slots /
    `798` spilled registers to `4` / `4`, ordinary-call GC-family appended
    slots dropped from `860` to `66`, total appended slots dropped from `1629`
    to `835`, and frame size dropped from `27936` to `24768` bytes.
  - Full LLVM self-stage install completed and the narrow five-module smoke
    passed, but this was not enough validation. A seven-module native-mode
    compiler workload caught a correctness failure: both the wrapper and real
    `_llvm_canon_stage1_install/bin/ocamlopt.opt` segfault while compiling
    `typing/typecore.ml`. The relocation-enabled relink reproduced the same
    failure, so this is not a BOLT/relink problem.
  - Benchmarking the same bad compiler on the narrower five-module set was
    also negative: native-built median `27.267752s`, candidate median
    `29.301929s`, ratio `1.074600`, improvement `-7.46%`
    (`canon_native_vs_llvm_pair_20260630_053516.json`).
  - The prototype was removed from the source tree and `_build/llvm-tools/bin/llc`
    was rebuilt cleanly from restored sources. The failed assumption is that an
    allocator spill slot already live across a statepoint necessarily contains
    the same current value as the listed register root. That is false: the slot
    may be live and value-typed but stale at that exact call, so replacing the
    register root with the slot can hide the only fresh copy from the GC.
  - Validation rule going forward: root-precision changes must pass at least
    the seven-module compiler workload (`cfg_selectgen`, `llvmize`,
    `translcore`, `ctype`, `env`, `typecore`, `typemod`) before any
    performance result is considered. Five-module smoke can miss real stale-root
    bugs.
- 2026-06-30 additional instrumented-BOLT screens:
  - Patched the previously unpatched
    `ocamlopt.constfilter.instrprof-cache-hfsort-peep-rodata-icp10.bat`
    artifact. Patching rewrote `224543` descriptors with zero unresolved and
    all by call-site mapping; `-version` passed. BOLT reported this top-10
    configuration optimized `0.0%` of indirect calls, so it is effectively a
    layout-only variant.
  - A one-sample/one-inner screen looked promising (`+4.64%`), but the normal
    seven-sample/three-inner benchmark rejected it as a goal candidate:
    native-built median `27.181370s`, candidate median `26.331605s`, ratio
    `0.968737`, improvement `+3.13%`
    (`native-current-vs-llvm-constfilter-instrprof-cache-hfsort-peep-rodata-icp10-inner3.json`).
    This is valid LLVM-built-binary-only work, but it is below the existing
    safe BOLT best (`+3.77%`) and below the required `+6%`.
  - Also tried merging the non-BOLT LBR profile
    `ocamlopt.constfilter.reloc.noassert.lbr.fdata` with the new
    instrumentation profile `ocamlopt.constfilter.instrumented-hot-bat.fdata`.
    `merge-fdata` accepted both as same-input legacy fdata and produced
    `ocamlopt.constfilter.reloc.lbr-plus-instrprof.fdata`. The same safe BOLT
    recipe then covered more profiled functions (`7805` vs `7350`/`7380`) and
    patched cleanly with zero unresolved descriptors.
  - The merged-profile artifact was rejected by a one-sample/one-inner screen:
    native `9.097661s`, candidate `9.125639s`, ratio `1.003075`, improvement
    `-0.31%`
    (`native-current-vs-llvm-constfilter-lbr-plus-instrprof-cache-hfsort-peep-rodata-screen1.json`).
    Do not spend a robust benchmark run on this merged profile.
  - Current best valid artifact remains
    `ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched` at `+3.77%`.
    BOLT profile tweaks and measured ICP variants are not closing the remaining
    gap; the next useful work should return to AMD64 LLVM root/spill precision
    or exact BOLT metadata for transformations that actually change hot code
    without corrupting OCaml frame roots.
- 2026-06-30 LLVM-path-only scope correction and BOLT old-sequence ICP:
  - Reconfirmed the review constraint: `-O4` and similar generic optimization
    level changes do not count toward the goal, since they can also apply to
    the native-built compiler. Candidate wins must be LLVM-backend/codegen,
    LLVM-path pass configuration, or LLVM-built-binary-only BOLT/profile work.
  - Improved the benchmark-local frametable patcher so BOLT ICP experiments
    with `--icp-old-code-sequence` can be evaluated instead of failing at the
    first missing descriptor:
    - `calls_in_range` and `shared_returns_in_range` no longer copy huge
      callsite suffixes for every function, making full-binary call mapping
      finish in the normal objdump-scale time.
    - BAT input-PC inversion is indexed for descriptor fallback mapping.
    - For ICP shapes where a fallback indirect call returns to a `jmp` into a
      shared continuation, the patcher maps the original descriptor to the
      shared continuation and appends a synthetic duplicate descriptor for the
      fallback return PC. Pairing uses order-preserving shared continuations
      against old indirect calls, because BOLT can map all inserted ICP code
      back to an earlier block address.
  - Repatched
    `ocamlopt.constfilter.cache-hfsort-peep-rodata-icp-oldseq.bat.bolt` with
    `--synthesize-icp-descriptors`. The final patch reported `224543` patched
    descriptors, `0` unresolved, `224452` call-site mappings, `91` BAT fallback
    mappings, and `284` synthesized ICP fallback descriptors. Startup
    `-version` passed.
  - Rejected this old-code-sequence ICP artifact before benchmarking. A focused
    native-mode compile of `backend/cfg_selectgen.ml` first exposed missing
    descriptors at generated ICP continuations; after fixing those mappings,
    the same smoke segfaulted. GDB stopped in
    `camlIkind__ckind_of_jkind_15_128_code`, with the stack passing through
    the newly descriptor-covered `camlMode__hint_407_1756_code` return
    continuation `0x3a996f3`. This is wrong-root or transformed-code corruption,
    not a valid performance candidate.
  - Conclusion: BOLT old-code-sequence ICP is now better diagnosed but still
    not safe enough for the compiler workload. Do not benchmark it or count it
    toward the `+6%` goal. The best valid result remains the safe BOLT layout
    artifact at about `+3.77%`; further progress still needs exact OCaml-aware
    metadata for real BOLT ICP or a backend root/spill precision improvement.
- 2026-06-30 rejected unobserved sibling-slot filter:
  - Reaffirmed the scope correction from review: `-O4` and other generic
    driver-level flags are invalid for the `+6%` goal because the native-built
    compiler can use the same flags. Candidate wins must be LLVM-backend
    codegen/pass changes, LLVM-path-only configuration, or post-link/profile
    work that applies only to the LLVM-built artifact.
  - Tested an LLVM-only `OxCamlStatepointSpillRoots` precision prototype that
    tried to avoid appending a sibling spill slot when no later load or later
    statepoint could observe the slot contents before an overwrite. The idea
    was to reduce AMD64 frametable/root pressure without changing frontend root
    production or adding a second GC mechanism.
  - Focused checks initially looked promising: rebuilt `_build/llvm-tools/bin/llc`;
    compiling `typing/ctype.ml` with the standard compiler, `-llvm-backend`,
    and patched `llc` passed; `-mllvm -stats` on `ctype` reported `1505`
    "unobserved spill slots not appended" versus `1504` remaining appended
    spill slots. The seven-module LLVM-backend compile smoke
    (`cfg_selectgen`, `llvmize`, `translcore`, `ctype`, `env`, `typecore`,
    `typemod`) also passed.
  - Full compiler build validation rejected the prototype. A separate
    LLVM-built install attempt using `_llvm_obsfilter_*` failed while building
    the main compiler: first with segfaults in `otherlibs/dynlink` and
    `middle_end/flambda2/kinds/flambda_kind.ml`; after broadening observations
    to include noncanonical frame-index loads, `flambda_kind` progressed but
    dynlink still failed with `Fatal error: allocation failure during minor GC`.
  - The prototype was reverted completely and `_build/llvm-tools/bin/llc` was
    rebuilt from the restored sources. Conclusion: LiveStacks intervals plus a
    local "future observation" test are not enough to decide that a sibling
    slot may be skipped. The missing observation may be through a path or
    machine construct not represented by canonical reload/statepoint uses, or
    the slot may be needed to keep root state coherent across exceptional or
    compiler-generated paths. Do not revive this filter without first proving
    the access model against the failing dynlink case.
- 2026-06-30 rejected global LLVM-only spill-fusing suppression:
  - Reaffirmed that generic `-O4`/optimization-level changes are not valid for
    the compiler-throughput goal because the native-built compiler can use them
    too. This experiment instead used an LLVM-path-only `llc` switch:
    `-mllvm -disable-spill-fusing`, appended by a local wrapper around the
    normal `tools/llvm-rs4gc-llc-wrapper.sh` pipeline.
  - Focused loop-invariant screening still confirms the earlier diagnosis. With
    the corrected real wrapper pipeline (`SAMPLES=5`, `N=12000000`, `REPS=5`),
    `loop_invariant_gc_across_call_dynamic_reps` moved to parity:
    native `0.0708s`, LLVM `0.0709s`, ratio `1.0005`; the fixed-reps GC case
    improved to ratio `1.0785`. This shows the microbenchmark's large AMD64
    slowdown is genuinely tied to X86 spill folding around statepoints, not to
    frontend roots or a generic optimization-level setting.
  - Full compiler-build validation rejects the global knob. A separate
    LLVM-built install attempt with `_llvm_nospillfuse_*` and the same wrapper
    first failed under `DUNE_BUILD_FLAGS=-j2` after `380` wrapper invocations:
    `.ocamlcommon.objs/native/build_path_prefix_map.cmx` got `SIGSEGV`, and
    `otherlibs/dynlink/.dynlink_compilerlibs.objs/byte/...Type_shape.cmo`
    aborted with `Fatal error: allocation failure during minor GC`.
    Rerunning the two apparent failing commands in isolation succeeded, so this
    was not enough by itself; a serialized main-build retry with
    `BUILD_RUNTIME=0 BUILD_MAIN=1 REFRESH_INSTALL=0 DUNE_BUILD_FLAGS=-j1`
    still failed after `266` wrapper invocations when compiling
    `.ocamlcommon.objs/native/typecore.cmx` (`SIGSEGV`). A clean serialized
    main build from scratch (`rm -rf _llvm_nospillfuse_main_build
    _llvm_nospillfuse_install`, reuse the completed runtime, then `-j1`)
    still failed after `540` wrapper invocations, with a dynlink native compile
    aborting on minor-GC allocation failure and later `.ocamlcommon` native
    compiles taking `SIGSEGV`.
  - Conclusion: broad spill-fusing suppression is correctness-unsafe for the
    self-stage compiler and is not a `+6%` candidate. The useful fact to carry
    forward is narrower: harmful post-statepoint scalar reload folding is a real
    performance class, but any fix must be targeted enough to preserve the
    compiler build and the existing statepoint/frimetable/root mechanism.
- 2026-06-30 LLVM-path-only scope checkpoint:
  - Reconfirmed the review constraint again: `-O4` and similar generic
    optimization-level or driver changes are not valid progress toward this
    goal, because the native-built compiler could receive the same change. Do
    not count such results. Valid candidates must affect only the LLVM path:
    AMD64 LLVM backend/codegen changes, LLVM-only pass configuration in the
    backend pipeline, or post-link/profile work on the LLVM-built compiler
    artifact.
  - The best valid compiler-throughput result remains the safe BOLT layout
    artifact
    `agent-state/test-suite-29e4cd/bolt_compiler_20260629/ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched`,
    about `+3.77%` versus the native-built compiler. Additional valid BOLT
    screens after the correction did not beat it: ctype-weighted compatible
    profile merge (`+2.79%` one-sample), `-align-functions=64` (`+2.70%`),
    `-align-blocks` (`+3.15%`), `-align-macro-fusion=all` (`-1.17%`), and
    `-use-edge-counts` (`+1.70%`).
  - The most promising remaining LLVM-only direction is still AMD64
    root/spill precision, not another generic optimization flag. For
    `typing/ctype.ml`, current stats show `1618` GC-family spill slots appended
    by `OxCamlStatepointSpillRoots`, split as `757` alloc-family, `860`
    ordinary-call, and `1` C-call. The old no-inplace diagnostic drops this to
    `200`, but it changes statepoint lowering shape and allocates `3098`
    statepoint stack slots instead of `114`, so it is a rejected diagnostic, not
    a target mechanism.
  - ARM comparison: this branch already uses the ARM-style in-place statepoint
    policy for AMD64 alloc and ordinary managed calls. The remaining AMD64
    penalty appears downstream, in x86 register allocation/spill folding
    creating sibling stack locations that must be listed for correctness.
    Broad post-RA filters such as "skip unobserved slots" and global
    `-disable-spill-fusing` are rejected because full compiler-build validation
    found GC/allocation failures. The next fix must prevent or narrowly reshape
    the bad AMD64 spill pattern while preserving the existing frametable/root
    mechanism, not filter roots away after the fact.
- 2026-06-30 rejected narrow x86 GC-spill-folding diagnostic:
  - Tested a narrower version of the previously rejected global
    `-disable-spill-fusing` idea. The temporary X86 diagnostic blocked ordinary
    frame-index memory folding only when the folded operand was an OxCaml GC
    pointer virtual register in an OxCaml function; statepoint operand folding
    and non-GC x86 folding were left alone. The source change was reverted
    after the measurement and `_build/llvm-tools/bin/llc` was rebuilt back to
    checked-in sources.
  - On the saved `typing/ctype.ml` post-RS4GC IR, direct `llc` stats showed
    the diagnostic did fire (`8136` attempted GC pointer spill folds blocked),
    but it did not move the root-pressure metrics at all. Baseline and
    diagnostic both reported `1618` GC-family spill slots appended (`757`
    alloc-family, `860` ordinary-call, `1` C-call), `9` crossing GC registers,
    `11` reload-fed sibling slots, and `1629` total spill slots appended.
    Regalloc merely shifted from `7133` to `7089` folded stack accesses and
    from `5938` to `5982` reloads inserted.
  - Conclusion: the compiler-throughput `ctype` gap is not solved by blocking
    ordinary x86 memory folding for GC vregs after allocation. The next
    LLVM-only investigation should target why the GC-family spill slots are live
    across statepoints in the first place, or reduce frametable/root descriptor
    volume in a way that is provably equivalent to the ARM/native mechanisms.
- 2026-06-30 `ctype` GC-family root producer classification:
  - Parsed the current verbose `OxCamlStatepointSpillRoots` log
    (`root_verbose_20260630/current/stderr.log`). It matches the current stats:
    `1638` appended locations total, split as `1629` stack slots and `9`
    register roots across `896` statepoints in `257` functions.
  - Top appended-location producers are concentrated in `ctype` unification
    code: `unify_row_field` (`105` locations), `unify_row` (`64`), `unify3`
    (`59`), `copy` (`54`), `instance_prim_locals` (`52`), `build_subtype`
    (`44`), and `loop_386` (`42`). ID `0` ordinary-call statepoints account
    for `767` appended locations, but encoded allocation/debug IDs also matter:
    `196609` has `176`, `327681` has `105`, `262145` has `78`, `131073` has
    `68`, and `393217` has `60`. The issue is therefore not isolated to either
    ordinary calls or allocation statepoints.
  - Parsed the emitted `camlCtype__frametable` descriptors and mapped return
    labels back to functions. Current `ctype.s` has `4528` descriptors and
    `20675` live roots. The no-inplace diagnostic has roughly the same
    descriptor count (`4530`) but fewer live roots (`19257`), so its improvement
    is genuinely root-list pressure rather than descriptor-count removal.
  - The largest current-minus-no-inplace live-root reductions are in the same
    hot functions: `unify_row_field` `461 -> 380` (`-81`), `unify_row`
    `728 -> 676` (`-52`), `instance_prim_locals` `192 -> 142` (`-50`),
    `build_subtype` `1115 -> 1071` (`-44`), `copy` `362 -> 320` (`-42`),
    and `loop_386` `487 -> 447` (`-40`). `unify3` dominates total descriptor
    roots (`2109`) but only drops by `20`, so the root-pressure fix should start
    with the duplicated stack homes in `unify_row_field`/`unify_row` rather
    than descriptor layout or generic BOLT.
  - Example `unify_row_field` statepoints repeatedly append the same stack
    homes across both ordinary calls and allocation/debug IDs: `%stack.1`,
    `%stack.3`, `%stack.11`, `%stack.15`, `%stack.19`, `%stack.21`, `%stack.25`,
    and `%stack.31` appear in several large appends. This supports the next
    source-level hypothesis: AMD64 is creating multiple long-lived stack homes
    for the same GC values inside hot functions, and the safe fix is to prevent
    or coalesce those duplicate homes before frametable emission, not to remove
    already-live roots after `OxCamlStatepointSpillRoots`.
- 2026-06-30 corrected compiler-throughput scope and root-mechanism checkpoint:
  - Generic optimization-level changes such as `-O4` are invalid for the
    compiler-throughput goal, because the native-built compiler can receive the
    same driver/runtime setting. The only valid candidates are LLVM-path-only
    changes: AMD64 LLVM backend/codegen improvements, LLVM-only backend pass
    configuration, or profile/post-link work that applies specifically to the
    LLVM-built compiler artifact.
  - The current best valid artifact remains the BOLT-patched LLVM-built
    compiler
    `bolt_compiler_20260629/ocamlopt.constfilter.cache-hfsort-peep-rodata.bat.patched`,
    about `+3.77%` versus the native-built compiler on the representative
    compiler-module benchmark, still short of the required `+6%`.
  - Rechecked the native AMD64/ARM and LLVM mechanisms. Native AMD64 and ARM
    both record register roots in frame descriptors, and both treat ordinary
    OCaml calls as destroying all physical registers. LLVM's statepoint path
    still needs `FixupStatepointCallerSaved` because register allocation cannot
    directly model statepoint GC operands as runtime "late reads"; that pass is
    shared infrastructure, not an AMD64-only replacement mechanism.
  - The AMD64-specific quality gap is downstream shape: `FixupStatepointCallerSaved`
    spills register roots that survive to ordinary calls, and
    `OxCamlStatepointSpillRoots` then appends sibling spill slots that also
    carry those GC values across statepoints. The large `ctype` counts are
    therefore mostly duplicate live homes created around the correct
    ARM-style in-place statepoint mechanism. The next LLVM-only fix should make
    AMD64 register allocation/fixup produce fewer duplicate homes, or prove a
    sound pre-frametable coalescing rule. Broad filters already tested
    (`-disable-spill-fusing`, skipping roots, allowing GC pointers in CSRs) are
    rejected because they either fail full compiler validation or do not reduce
    the relevant root pressure.
