# Vox limitations hit while building the WebAssembly differential test (27 Sep 2026)

1. **Plain integers into refined types need a runtime guard.** Building a
   `Wasm_u32.bytes` from an OCaml string, `B.Byte (Char.code s.[i], acc)` is
   rejected ("Refinement could not be proved (counterexample: i = 0)") because
   the checker does not know that `Char.code` returns a value in 0..255.
   Workaround: `let byte n : B.byte = if 0 <= n && n < 256 then n else invalid_arg "byte"`.
   Suggested fix: give `Char.code` (and `Bytes.get`/`String.get` composed with
   it, `input_byte`) refined result types in the checker's view of the stdlib.

2. **Incremental `./dev test` rejects the `script` action.** A test that should
   be skipped when Node is absent cannot use `script = "..."; script;` ("Incremental
   testing does not support action "script""). Workaround: a builtin predicate
   `has-node` in ocamltest/builtin_actions.ml, next to `has-z3` (both now use
   an `on_path` helper). Suggested fix: allow `script` incrementally when it
   produces no artifact, or provide a generic `has-program` predicate.

3. **`./dev test --promote` does not create a missing reference.** With no
   `.reference` file the run fails with "expected to be empty because there is
   no reference file" and nothing is written; the reference had to be copied
   by hand from `_build/vox-dev-runs/.../*.opt.output`. Suggested fix: promote
   into a new reference file too.

4. **No way to observe a run's step count from the model's public runner.**
   `Wasm_binary_execution.run` returns only the final state, so counting which
   instructions the model executed needed a copy of its loop
   (`Wasm_calls.start` then `Wasm_calls.step`). Not a checker issue; a
   `run` variant that also returns the fuel left would make coverage and
   no-verdict diagnostics cheaper.
