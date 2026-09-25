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

`Vox_rsa.Modular.power a e` specifies ordinary integer exponentiation by
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

`Vox_rsa.Number_theory.prime p` means `p > 1` and no integer in `[2,p-1]`
divides `p`. The predicate executes trial division; it does not include an RSA
or Fermat test. `prime_divisors` derives the usual divisor characterization.
`lambda_lcm` proves that `lambda p q` is the least positive common multiple
of `p-1` and `q-1`. Input validation is deliberately simple and takes linear
work in the prime candidate; the examples are not a practical key generator.

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
```

The fixture includes a statically proved non-coprime example, differential
modular-power tests, all messages and valid exponents through 24 for prime
pairs chosen from `{2,3,5,7,11,13}`, every message modulo `53*61`, non-coprime
examples modulo `65537*257`, exponents over 100 decimal digits, and CRT
equivalence checks. Bytecode and native execution are tested. The current compiler rejects a Bigint access-mode
coercion in the ghost proof under `-principal`; these tests omit that flag.

`rsa_rejected.ml` rejects invalid preconditions, including a composite factor.
Runtime `assume_` checks in the test harness validate input preconditions only;
they are not part of the implementation or its correctness proof.
