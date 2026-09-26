# Implicit refinements in general examples

Remove 1,341 source occurrences of `refine_` from 54 general example and
fixture files, plus seven occurrences emitted by the two benchmark generators.
The inventory records source counts and SHA256 values. The base is PR #208 at
`4446484d2db175a1b8da2041f57063f2f4b56d53`, on the shared-library prerequisite
PR #207. No assumptions are added. Public predicates, ghost scopes and rejection
classes are preserved. Ordinary local bindings remain where this migration does
not establish that removing them preserves verification.

The 45 ocamltest roots listed in the inventory pass their declared bytecode,
native and principal variants. They cover the remaining seven support modules
through their dependency headers; the HOF benchmark additionally compiles all
four HOF units and generated clients with both compilers. Rejection diagnostic
spans and inferred output were refreshed without promoting new error classes or
unused-variable warnings. The two final window/environment examples also pass
through the committed runner:

```
python3 verification/review/check_implicit_examples.py COMPILER_ROOT
```

`COMPILER_ROOT` must contain an existing `_install`, `_runtest/ocamltest/ocamltest`,
and `_build/main/oxcaml/testsuite/tools/{expect,expectnat}.exe`. The runner writes
only this checkout's `_build/implicit-examples`; it neither rebuilds nor changes
the compiler installation. The installation used here is the read-only
`worktrees/time-credits` installation pinned in `implicit-library.md`.

`borrow_parallel.ml` compiles in both backends, with and without principal.
The installed test harness skips its multicore predicate. Direct execution
fails to allocate a domain, so its parallel runtime behavior is not counted
as verified by this installation.

The existing `pref_records.ml` callback-payload allocation fails with
"Unsupported refinement predicate in VC generation" on that compiler.
Rebuilding the original Pref implementation/interface and original fixture in
isolation reproduces the failure. Its migrated source retains the same failure;
it is recorded separately and is not counted among passing tests.

Generated HOF programs were compiled and executed using:

```
python3 verification/benchmarks/hof_idioms.py COMPILER_PREFIX --repetitions 1
```

Both generated quicksort client variants were compiled against the public
interfaces in both backends. No quicksort performance comparison was run or
claimed. The quicksort iarray runtime/client matrix passes independently.

Current README and programming guides describe implicit introduction and
elimination, expected types for nested refinements, and stable-local function
adaptation. Language regression/explicit-syntax fixtures and dated historical
reports are preserved. This commit does not claim that other owners' HM,
Incremental or mode-solver branches have been fully migrated or published.
