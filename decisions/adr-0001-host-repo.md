# ADR-0001 — Host repository

Status: accepted · 2026-07-10

## Context

DESIGN.md §11 envisions `main/` as a clone of the personal oxcaml GitHub fork
with a long-lived `oxsmt` branch as integration trunk. This environment has no
fork/push credentials and no network in the dev loop.

## Decision

Development starts in a **fresh local git repository** at
`/usr/local/home/jujacobs/oxsmt/main`, branch `oxsmt`, with no remote. Commits
stay local.

## Rationale

- `smt/` is self-contained and stdlib-only (I3), so it is not coupled to the
  compiler tree. Migrating onto the fork later is a cheap `git subtree`/copy.
- The design already treats the host repo as "a convenient host, not a
  participant" (DESIGN.md §1) — the compiler is never built or run here — so
  nothing of value is lost by deferring the fork clone.

## Consequences

- No push/integrator-push step exists yet; the integrator agent (DESIGN.md §11)
  merges locally and fast-forwards `oxsmt`.
- Upstream-sync janitor tasks are deferred until a remote exists.

## Revisit

At the compiler-integration milestone (DESIGN.md §9), when build-graph wiring and
`-dump-smt` land and a real fork/remote is available.
