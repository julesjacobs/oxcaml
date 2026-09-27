title: Textbook RSA
blurb: Modular exponentiation on unbounded integers, proved to compute `a^e mod n`, and the RSA round trip proved for any two distinct primes and any message below their product.
status: review-pending
date: 27 September 2026
sources:
  - verification/library/vox_rsa.mli — Public interface
  - verification/library/vox_rsa_spec.ml — Definitions of `power`, `prime`, `gcd`, `lcm`, `lambda` and `valid_key`
  - verification/library/vox_rsa_spec.mli — The same definitions as checked equations
  - verification/library/vox_rsa_arithmetic.ml — Repeated squaring (`modexp`) and the exponent and remainder laws
  - verification/library/vox_rsa_number_theory.ml — Extended Euclid, Bézout coefficients, lcm and CRT uniqueness
  - verification/library/vox_rsa_fermat.ml — Fermat's little theorem
  - verification/library/vox_rsa.ml — The round-trip theorem and CRT decryption
  - verification/library/vox_rsa.md — Reading order and proof outline
  - testsuite/tests/vox/rsa_public_client.ml — Public-only client
  - testsuite/tests/vox/rsa_rejected.ml — Rejected clients
  - testsuite/tests/vox/check_rsa_boundary.py — Compiles the client with only the public interfaces
---
`Vox_rsa` computes modular powers of unbounded integers (`Bigint.t`) by repeated squaring. `modexp a e n` is proved to return `power a e mod n` for every integer `a`, every `e >= 0` and every `n > 0`, where `power` is repeated multiplication; `encrypt` and `decrypt` are the same function. `decrypt_crt c d p q` computes the residues modulo two distinct primes and recombines them, and is proved to return `power c d mod (p * q)` for every `d >= 0`. The theorem `roundtrip_correct` states that decrypting the encryption of `m` gives back `m` for any distinct primes `p` and `q`, any `e, d > 0` with `(e * d - 1) mod lcm (p - 1) (q - 1) = 0`, and any `0 <= m < p * q`, including messages divisible by `p` or `q`. Primality is defined by trial division. Fermat's little theorem, Bézout's identity and the uniqueness part of the Chinese remainder theorem are proved in the library; it adds no axioms.

This is the arithmetic of textbook RSA only: there is no key generation, efficient primality test, padding or signature scheme, and no claim about security, constant-time execution or running time. There are no size bounds and no error results: the operations have preconditions instead, and `roundtrip_correct` states a conditional theorem about any integers.

## Client example

The public-only client. `open Bigint` makes `t` the type `Bigint.t` and the arithmetic operators act on it; `0Z` is a `Bigint.t` literal. `(f @ total)` declares that `f` terminates without effects, and `{m : t | p}` is the type `t` refined by the predicate `p`. `ghost_ (...)` is proof code, checked and then erased. `Spec.valid_key_def` and `Spec.prime_def` state the definitions of `valid_key` and `prime`; the client calls them so that the checker can prove `n > 0Z`, which `encrypt` requires. `(() : {u : unit | p})` asks the checker to prove `p` at that point.

@code testsuite/tests/vox/rsa_public_client.ml "open Bigint" "  crt"

## A rejected program

A zero modulus violates `modexp`'s precondition `n > 0Z`. The test is an expect test: the expected compiler output follows the program.

@code testsuite/tests/vox/rsa_rejected.ml "let zero_modulus () =" "|}]"

## Interface

@code verification/library/vox_rsa.mli

`Spec` is `Vox_rsa_spec`, whose definitions are:

@code verification/library/vox_rsa_spec.ml

`let[@def]` also generates the lemmas `power_def`, `prime_def` and so on, which state each definition's equation; `[@@decreases e]` gives the termination measure. `Bigint`'s `/` and `mod` are Euclidean, so `mod` by a positive number is never negative.

## Trusted base

Nothing beyond the shared base.

## Scope

- Operations: `modexp`, `encrypt`, `decrypt`, `decrypt_crt`, the theorem `roundtrip_correct`, and `roundtrip`, which runs encryption and decryption and returns a value proved equal to its input.
- `decrypt_crt` uses the full exponent modulo each prime; it does not reduce it modulo `p - 1` and `q - 1` as CRT implementations usually do.
- `roundtrip_correct` returns its result `@ ghost`, so its proof is erased and never computes `power m (e * d)` at run time.
- `modexp` branches on the bits of the exponent; the number of squarings is logarithmic in the exponent, but no running-time theorem is stated.
- `@@ total` means termination in the checker's model; memory and time are not bounded.

## Reproduce

After `./configure --prefix=$PWD/_install`, `make install` and `./dev init`:

```
./dev test vox/rsa.ml vox/rsa_rejected.ml vox/rsa_public_client.ml
python3 testsuite/tests/vox/check_rsa_boundary.py
```

`rsa.ml` checks the library and runs differential tests of `modexp`, round trips of every message for each pair of distinct primes from 2, 3, 5, 7, 11 and 13 and every valid `e` and `d` up to 24, every message modulo `53 * 61`, and exponents of more than 100 decimal digits, as bytecode. These tests establish each call's precondition with `(assume_ x : {v : t | p})`, which checks `p` at run time. `rsa_public_client.ml` is compiled as bytecode and native code. `check_rsa_boundary.py` compiles the client with only `vox_rsa.cmi` and `vox_rsa_spec.cmi` available, then links and runs it with both compilers. The client only defines functions, so these two check compilation and linking, not the client's results.
