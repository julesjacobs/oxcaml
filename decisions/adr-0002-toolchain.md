# ADR-0002 — Toolchain

Status: accepted · 2026-07-10

## Context

The project targets pure OxCaml (DESIGN.md §1), and §4 poses an open question:
do OxCaml-only features (unboxed types, immutable arrays) pay their way in the
term representation? No OxCaml switch is installed here; the available compiler
is stock OCaml 5.4.0.

## Decision

- Build against **stock OCaml 5.4.0** (dune 3.20.2, ocamlformat 0.29.0, menhir)
  at `/usr/local/home/jujacobs/.opam/5.4.0/bin`.
- Write **portable-subset OCaml**: no OxCaml-only features for now. The §4
  representation question is **deferred** and will be answered against a real
  OxCaml switch later.
- The oracle is **Lean 4.31.0** via nix (`~/.dispatch/bin/lean`, provided by
  `dispatch add-nix`); `by grind` availability is verified.

## Rationale

Stdlib-only, portable-subset code compiles on both toolchains, so the firewall
(I3) keeps us free to adopt a real OxCaml switch later without a rewrite. Picking
OxCaml-only representations now would be premature optimization against an
unavailable compiler.

## Consequences

- The bare `dune` on PATH is a Jane Street dispatch wrapper that fails outside
  jane workspaces; all entry points pin the opam path (AGENTS.md, Makefile).
- Any future OxCaml-feature adoption is its own ADR with a measured case.

## Revisit

When an OxCaml switch is available and the term representation (ADR-0003) is
benchmarked against it.
