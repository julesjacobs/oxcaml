# Textbook RSA with erased proofs

`Vox_rsa` computes modular powers by repeated squaring. `encrypt` and `decrypt`
use that implementation; `roundtrip` executes encryption followed by decryption
and returns a value refined to equal the input message. `decrypt_crt` has the
same modular-power specification as ordinary decryption.

## Domain and public specifications

All arithmetic uses Vox's unbounded `Bigint.t`, including exponents,
multiplication, division, and remainders. There is no machine-integer arithmetic
in the implementation and no fixed bound on the primes, exponents, or modulus.
Bigint primitives and their mathematical SMT encoding belong to Vox's trusted
base; this demo does not verify their C implementation.

`Vox_rsa.Spec.power a e` specifies ordinary integer exponentiation by
multiplication, with `power a 0 = 1`. For every integer base, nonnegative
exponent, and positive modulus, the executable operation guarantees:

```
0 <= modexp a e n < n
modexp a e n = power a e mod n
```

The RSA theorem applies to **any distinct primes** `p` and `q`, positive
exponents `e` and `d` satisfying `(e*d - 1) mod lcm(p-1,q-1) = 0`, and every
`0 <= m < p*q`. This includes zero and multiples of either prime. The public
[interface](vox_rsa.mli) states the result refinements and theorem contracts.

`Vox_rsa.Spec.prime p` means `p > 1` and no integer in `[2,p-1]`
divides `p`. Its definition is trial division, independent of RSA and Fermat. `Spec.valid_key`
exposes exactly the prime, distinctness, positivity, and inverse premises above.
`Spec.lcm` uses Euclid's `Spec.gcd`; the implementation proves its
common-multiple and leastness properties.

## Exact transitive human-review surface

Read these files in order to audit the meaning of every public claim:

1. [`stdlib/bigint.mli`](../../stdlib/bigint.mli): unbounded integer primitives,
   their arithmetic contracts, and the division/remainder convention.
2. [`vox_rsa_spec.ml`](vox_rsa_spec.ml): complete definitions of `power`,
   `no_divisors`, `prime`, `gcd`, `lcm`, `lambda`, and `valid_key`.
   `gcd` is a semantic dependency of `lcm`; `no_divisors` is a semantic
   dependency of `prime`. Both are included in this review surface.
3. [`vox_rsa_spec.mli`](vox_rsa_spec.mli): the sealed semantic API and complete
   checked definition equations for each of those definitions.
4. [`vox_rsa.mli`](vox_rsa.mli): contracts for the actual executable operations
   and the encryption/decryption composition theorem `roundtrip_correct`.
5. This document: domain, termination, erasure, and trust conventions.

These claims additionally trust Vox's refinement checker, its SMT encoding and
solver, ghost erasure, and the underlying compiler/runtime. `@@ total` requires
termination in Vox's model; it does not promise enough physical memory or time.
No additional arithmetic axioms or trusted RSA primitives are introduced.
The public signature aliases only the sealed semantic module. It exposes no
permutation, product, Fermat, Bézout, or induction machinery. The four
implementation files below are outside the semantic review surface: their
proofs and executable results are checked against the public contracts.

## Proof structure

[`vox_rsa_arithmetic.ml`](vox_rsa_arithmetic.ml) proves the exponent laws by
induction, the modular multiplication laws, and the exact refinement of
repeated squaring. Its termination measure is the exponent, halved at each
recursive call.

[`vox_rsa_number_theory.ml`](vox_rsa_number_theory.ml) verifies extended Euclid:
the returned gcd divides both inputs and has explicit Bézout coefficients.
Primality then gives modular cancellation. The same arithmetic establishes
the lcm's common-multiple and leastness properties and CRT uniqueness.

[`vox_rsa_fermat.ml`](vox_rsa_fermat.ml) proves Fermat's theorem for general
primes. Multiplication by an invertible residue permutes the nonzero residues.
The proof establishes this by equal element counts and a checked theorem that
equal counts imply equal products. It then cancels the nonzero product to
obtain `a^(p-1) mod p = 1`. The zero-residue case gives `a^p mod p = a mod p`
for every integer `a`. Induction extends the identity to
`a^(1+k*(p-1)) mod p = a mod p` for every `k >= 0`.

[`vox_rsa.ml`](vox_rsa.ml) combines the two prime congruences using CRT and
applies that theorem to the values returned by encryption and decryption.
CRT decryption computes both prime residues and recombines them with the
proved inverse of `p` modulo `q`. Its exponent is not reduced modulo `p-1`
or `q-1`, so zero exponents and messages divisible by a prime are handled
directly.

Calls used only for correctness are inside `ghost_`. Modular exponentiation,
roundtrip, and CRT decryption construct no runtime proof certificates,
evaluate no power or residue-list models, and perform no final correctness
checks. Bézout records and proof-only lists are erased with their calls.
Proof recursion can be linear in the exponent or prime; runtime modular
exponentiation uses logarithmically many squaring steps. This is not a
constant-time or bit-complexity guarantee.

This is arithmetic correctness of textbook RSA, with no security or
production-padding claim.

## Running

From a configured worktree:

```sh
./dev init
./dev test vox/rsa.ml
./dev test vox/rsa_rejected.ml
./dev test vox/rsa_public_client.ml
python3 testsuite/tests/vox/check_rsa_boundary.py
```

The fixture includes a statically proved non-coprime example, differential
modular-power tests, all messages and valid exponents through 24 for prime
pairs chosen from `{2,3,5,7,11,13}`, every message modulo `53*61`, non-coprime
examples modulo `65537*257`, exponents over 100 decimal digits, and CRT
equivalence checks. The public client runs in bytecode and native code; the
other tests run as bytecode. The current compiler rejects a Bigint access-mode
coercion in the ghost proof under `-principal`; these tests omit that flag.

`rsa_rejected.ml` rejects invalid preconditions, including a composite factor.
Runtime `assume_` checks in the test harness validate input preconditions only;
they are not part of the implementation or its correctness proof.

`rsa_public_client.ml` derives the general encryption/decryption composition
and CRT equivalence using only `Vox_rsa` and `Vox_rsa.Spec`.
`check_rsa_boundary.py` separately compiles it with only `vox_rsa.cmi` and
`vox_rsa_spec.cmi` as library interfaces, then links and runs it. No proof-module
interface is available while compiling the client.
