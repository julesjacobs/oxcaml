# LLVM Expect-Test Support

## Definition

LLVM expect-test support is an `expect.opt`-based testsuite mechanism for
small OCaml examples compiled with `-llvm-backend`. It captures normalized LLVM
backend output per evaluated phrase and compares it against inline
expectations.

It should support two output levels:

- `[%%expect_llvm_ir]`: the LLVM IR emitted by OxCaml.
- `[%%expect_llvm_asm]`: the final target assembly emitted by LLVM.

It should not replace existing `[%%expect]` or native-backend
`[%%expect_asm]` tests.

## Example Test

```ocaml
(* TEST
 flags += " -O3 -llvm-backend";
 only-llvm-backend;
 expect.opt;
*)

let add1 x = x + 1;;

[%%expect{|
val add1 : int -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|
add1:
  ; normalized LLVM IR here
|}]

[%%expect_llvm_asm AArch64{|
add1:
  ; normalized LLVM-produced assembly here
|}]
```

The inline expectations should contain normalized output, not raw compiler
output. The first implementation keeps only user code functions, normalizes
toplevel-generated names such as `camlTOP1` to `camlTOP`, and drops module/data
boilerplate. Broader target support will need either more architecture/system
filters or separate tests per target.

## Test Layout

```text
testsuite/tests/llvm-codegen/
  arithmetic.ml
  allocation.ml
  calls.ml
  control_flow.ml
  exceptions.ml
```

The first useful tests should be small:

- arithmetic and control flow;
- allocation or calls with safepoints;
- exception-handler setup/restore shape;
- final target assembly, not only LLVM IR.

## Implementation Shape

```text
oxcaml/testsuite/tools/
  expectcommon.ml        # parse/check expect_llvm_ir and expect_llvm_asm
  expectnat.ml           # register LLVM output callbacks and read OCAMLPARAM

backend/
  emit.mli               # keep existing native asm callback unchanged

backend/llvm/
  llvmize.ml             # capture and normalize LLVM code functions and asm
```

The clean design point is that the expected output lives inline with the OCaml
example, while the harness captures the LLVM backend's actual per-phrase output
from the compiler pipeline. It should not scrape a global `.s` file after the
compile.
