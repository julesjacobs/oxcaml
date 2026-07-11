# ADR-0004 — Task branch naming

Status: accepted · 2026-07-10

## Context

DESIGN.md §11 specifies task branches named `oxsmt/task/<name>`. Git cannot
hold both a ref `oxsmt` and refs under `oxsmt/` — a ref and a directory of the
same name collide in `.git/refs`, and the integration trunk is the branch
`oxsmt`.

## Decision

Task branches are named `task/<name>` (e.g. `task/harness`, `task/gate`),
dropping the `oxsmt/` prefix. This is already the de facto scheme.

## Consequences

DESIGN.md §11's `oxsmt/task/<name>` sketch is amended to `task/<name>`. No other
change to the branch-per-task, worktree-1:1 model.
