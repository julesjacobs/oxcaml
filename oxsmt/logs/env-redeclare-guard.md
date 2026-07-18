# Env declare_fun write-once — freeze report (task #63)

Branch `task/env-redeclare-guard` off trunk `be8e516b8b` (the landed symmetry stack).
A PRE-EXISTING, flag-independent DT soundness gap codex surfaced while confirming the
symmetry lane's R4 `set_datatypes` validator.

## The hole

`Env.declare_fun` did an unconditional `Hashtbl.replace` for non-reserved symbols
(last-wins), while `Env.declare_reserved` is write-once. So a caller could, AFTER a
validated `set_datatypes` installed a datatype registry, redeclare a datatype **constructor**
(registered at `() -> the datatype`) as an uninterpreted constant at a different rank
(`() -> U`). The DT theory keeps classifying that symbol as a constructor by registry
membership while its env rank now says another sort — its rules fire on a sort-mismatched
term → wrong verdict (constructor clash where SAT is right). The `set_datatypes` install-door
validator (R4) checks ranks AT INSTALL; this reopened the same class AFTER install via
`declare_fun`.

The SMT-LIB parser already rejects redeclaration at its own namespace layer
(`st.funs`/`st.defines` dedup), so this is only reachable through the direct
`Env`/`Session.declare_fun` API — but that is public (`env.mli` / `session.mli`).

## Fix

`Env.declare_fun` is now WRITE-ONCE, the user-named analogue of `declare_reserved`:
re-declaring at the IDENTICAL rank is idempotent; CHANGING an existing rank raises
`Invalid_argument`. Body-only change to `smt/core/env.ml` — no signature change, so the
FROZEN `env.mli` is untouched (`check-frozen` stays green).

### Scope note (flagged for review)

The lead's ruling offered a narrower, registry-scoped guard (reject rank-change only for
symbols in an installed datatype registry). I chose the broader **all-user-symbols**
write-once instead, because the registry-scoped version would require `Env.declare_fun` to
consult the datatype registry — which it does not hold — forcing a new function or a
signature change on the FROZEN `env.mli` (an unfreeze ritual on a core interface). The
broader guard: (a) is body-only, no frozen-interface change; (b) is sound — a rank-changing
redeclaration is malformed SMT-LIB regardless; (c) keeps every shipped path green (verified
below). If review prefers the registry-scoped version despite the `env.mli` unfreeze, say so.

## Gates (by EXIT CODE)

| gate | result |
|---|---|
| `make test` (full suite) | exit 0 |
| `make wiring-test` (adds the write-once test) | exit 0 (211 checks, 0 failures) |
| `make dt-sat-gate` | exit 0 |
| `make check-frozen` | exit 0 (14 intact; `env.mli` unchanged) |

### Test (RED-verified — tests/solver/wiring_test.ml, `make wiring-test`)

`test_declare_fun_write_once`: register a datatype `D` with constructor `C` via
`set_datatypes`, then redeclare `C` as an uninterpreted constant `() -> U` — REJECTED
(codex's exact trigger). Also checks an idempotent same-rank redeclaration is still allowed
(guards over-rejection). RED-verified: disabling the `declare_fun` guard makes the rejection
check FAIL ("expected an exception"). Well-formed paths unaffected: the parser dedups
redeclaration itself, and `make test` / `dt-sat-gate` are green.

## Status

Single commit, dark-neutral (this is a fail-closed guard on a malformed-input path; no
verdict changes on well-formed input). Frozen at the tip of `task/env-redeclare-guard`.
Single-leg review per the lead. Ends the registry-hole chase the symmetry lane surfaced:
two pre-existing DT soundness gaps (set_datatypes validation, declare_fun write-once) found
and closed.

## Pre-ON belt (folded in): symmetry breaking skips datatype-using sessions

Also on the symmetry pre-ON checklist (codex): a one-line `&& not (uses_datatypes t)` on the
symmetry-breaking emission guard (session.ml `assert_presolved`). This DECOUPLES symmetry
soundness from datatype-registry / Env well-formedness entirely — symmetry never emits on a
datatype-using session. B3 already excludes datatype-sorted candidates, so this only skips the
free (uninterpreted) constants of a mixed QF_UFDT problem; the measured win is pure QF_UF (no
datatypes), so it costs nothing (pure-QF_UF `uses_datatypes` is false — the guard is a no-op
there, win byte-identical). Gates re-run green: make test / symbreak-test (15) / wiring-test
(211) / check-frozen all exit 0. Committed separately on this branch.
