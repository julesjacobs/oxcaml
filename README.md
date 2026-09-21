# Vox

Vox adds checked refinement contracts, erased proofs, and ownership-aware
verification to OCaml. This repository includes its OxCaml compiler.

Start with [bounded clamp](testsuite/tests/vox/bounded_clamp.ml): the caller
establishes ordered bounds, and the function guarantees a result within them.
The client uses ordinary calls; no explicit proof wrappers are needed.

```ocaml
let (clamp @ total) (lo : int) (hi : {h : int | lo <= h}) (x : int) :
    {r : int | lo <= r && r <= hi} =
  if x < lo then lo else if x > hi then hi else x

let bounded (x : int) : {r : int | 0 <= r && r <= 10} =
  clamp 0 10 x
```

Continue with [checked windows](testsuite/tests/vox/checked_windows.ml) for
runtime validation, then [queue clients](testsuite/tests/vox/queue_client.ml)
for an abstract verified data structure. The
[executable tour](testsuite/tests/vox/README.md) covers the larger examples.
[Verification programming](design-docs/verification-programming.md) explains
how to implement contracts and reusable proofs.

After configuring with a worktree-local prefix, run `./dev init`, then
`./dev test vox/bounded_clamp.ml`. Verification requires Z3 on `PATH`; inspect
the test summary because solver-dependent tests skip when Z3 is absent.
Use the compiler's `-dvc` option to inspect a failed goal, its encoded
assumptions, model values, and opaque calls. Models involving opaque calls can
indicate missing facts rather than a runtime bug.

Integers retain OCaml's wrapping arithmetic. `assume_` validates at runtime;
Vox proves refinement contracts automatically. Explicit `refine_` is still needed
for some higher-order contract adaptations and nested refinements. Proof code wrapped in
`ghost_` is erased. See the [library guide](verification/library/README.md)
for the ownership and trusted-primitive boundaries.

## Underlying OxCaml compiler

A performance-focused version of OCaml.
This is also the home of the Flambda 2 optimiser and the Cfg backend.

OxCaml is currently based on OCaml 5.4 (plus some patches from later
upstream revisions, mainly in the runtime).

The following gives basic instructions for getting set up.  Please see
[`HACKING.md`](HACKING.md) for more detailed instructions if you want to develop in this repo.
That file also contains instructions for installing the OxCaml compiler in a way
that it can be used to build OPAM packages.

## One-time setup for dev work or installation

The supported platforms are x86-64 and arm64 Linux; and arm64 macOS.  x86 macOS may still work.

One-time setup:
```
$ opam switch create oxcaml-dev --empty
$ opam pin add -ny git+https://github.com/oxcaml/oxcaml
$ opam switch set-invariant -y --packages oxcaml-dev
$ eval $(opam env)
```

You can check that an existing opam switch is using the currently required versions of tools by
running:
```
$ opam upgrade oxcaml-dev
```

You probably then want to fork the `oxcaml/oxcaml` repo to your own Github org.

## Branching and configuring

Use normal commands to make a branch from the desired upstream branch (typically `main`), e.g.:
```
$ git clone https://github.com/oxcaml/oxcaml
$ cd oxcaml
$ git checkout -b myfeature origin/main
```

The OxCaml tree has to be configured before building.  The configure script is not checked
in; you have to run `autoconf`.  For example:
```
$ autoconf
$ ./configure --prefix=/path/to/install/dir
```

## Building and installing

To build and install OxCaml, which produces a compiler installation directory whose
layout is compatible with upstream, run:
```
$ make install
```
