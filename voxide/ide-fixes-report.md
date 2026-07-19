# voxide IDE fixes report

## What changed

### Identifier modes at the cursor

- `compiler.py` now translates the compiler dump's `identifier_modes` entries
  from 1-based/UTF-8 spans to 0-based/UTF-16 editor ranges.
- `/vcs` responses carry `identifier_modes` for single buffers. Workspace
  responses carry file-tagged ranges so the client can filter them to the
  active unit.
- `app.js` caches the ranges, clears them on buffer changes, and refreshes the
  CURSOR zone on both compiler responses and cursor movement.
- `pane_model.js` owns the cursor selection rule: choose the smallest
  containing type range and the smallest containing mode range independently.
  When both exist, the type is shown first and the compiler's mode string is
  shown verbatim on the next line.

Screenshot-in-text at the binder and read of `mode_x`:

```text
int
@ unique total stateless
```

No mode is inferred or reconstructed client-side. Off every compiler-emitted
identifier span, the existing honest placeholder remains:

```text
No inferred expression type at the cursor.
```

### Type errors in compact mode

- The diagnostics container is no longer globally tagged `depth-1`.
- Type/compile diagnostic buttons are status-level and therefore remain
  visible in compact mode.
- Verification diagnostic detail remains `depth-1`: compact may collapse that
  detail because its verdict is already carried by the status/proof surfaces.

Screenshot-in-text for a compact type error:

```text
header:      ✗ type error
diagnostic:  TYPE  Line 1, column 5: This expression has type bool but int was expected
proof:       Obligations unavailable — this buffer did not compile.
```

This preserves the pane's honesty rule: a compile error never becomes an
obligation verdict, and the status-level error is never dropped.

## JSON evidence from the modes-enabled compiler

The server request logic was exercised directly with the required `source`
field and the compiler at
`worktrees/modes-cursor/_install/bin/ocamlc.opt`. The environment prohibited
binding a listening socket, so this used `server.process_post` (the same request
decoder/router as HTTP) without touching the live `:8471` server.

Source:

```ocaml
let id (x : int) = x
let y = id 1
```

Relevant `/vcs` response projection:

```json
{
  "revision": 41,
  "unavailable": false,
  "vcs": [],
  "identifier_modes": [
    { "start": { "line": 0, "col": 4 }, "end": { "line": 0, "col": 6 }, "mode": "@ unique total stateless" },
    { "start": { "line": 0, "col": 8 }, "end": { "line": 0, "col": 9 }, "mode": "@ unique total stateless" },
    { "start": { "line": 0, "col": 19 }, "end": { "line": 0, "col": 20 }, "mode": "@ unique total stateless" },
    { "start": { "line": 1, "col": 4 }, "end": { "line": 1, "col": 5 }, "mode": "@ unique total stateless" },
    { "start": { "line": 1, "col": 8 }, "end": { "line": 1, "col": 10 }, "mode": "@ unique total stateless" }
  ]
}
```

The matching `/check` response returned ordinary type ranges, including `int`
for `x`; the frontend regression verifies the combined two-line CURSOR output.

A real two-file workspace compile also returned correctly tagged mode ranges:

```json
{
  "ok": true,
  "identifier_modes": [
    { "file": "Demo.ml", "start": { "line": 0, "col": 8 }, "end": { "line": 0, "col": 9 }, "mode": "@ unique total stateless" },
    { "file": "Client.ml", "start": { "line": 0, "col": 8 }, "end": { "line": 0, "col": 15 }, "mode": "@ unique total stateless" }
  ]
}
```

## Verification

Passing checks:

- `python3 -m unittest tests.test_compiler tests.test_server tests.test_workspace tests.test_view_harness`
  — 62 tests passed, 5 compiler-dependent tests skipped under the default
  unbuilt worktree.
- Targeted real-compiler mode tests with `VOX2_OCAMLC` set to the modes-enabled
  binary — 4 tests passed, including binder/read spans.
- `node tests/test_frontend.js` — all frontend checks passed, including:
  binder mode, read mode, off-span honesty, and compact type-error visibility.
- JavaScript syntax checks for the app, shared model, frontend test, and
  fidelity test passed.
- `git diff --check` passed.
- Fidelity core (the test's nested-process sections omitted because this
  sandbox denies Node `spawnSync`) passed across 2,012 carets in each mode:
  shared model == browser DOM for body, mode, legend, and CURSOR readout; the
  cursor map also resolved exactly to the point-query pane.
- Direct terminal-tool smoke check for `binder.ml` produced:

  ```text
  ⊢ x > 0  ✓
  7 = 7
  x = 7
  ```

The unmodified `node tests/test_pane_fidelity.js` could not complete in this
managed sandbox because Node child-process creation is denied with `EPERM`
(first at its `git show` baseline, and likewise for nested CLI invocations).
The code path that does not require child processes passed as described above.

For completeness, the full Python suite was also run against the alternate
modes compiler: 82 of 84 runnable tests passed. The two failures were existing
workspace expectations specific to the other compiler branch (`Demo.mli` seal
attribution/status); the new identifier-mode unit and real-compiler tests all
passed. The default worktree suite is green as listed above.

Repository-wide build checks were attempted. `make -s boot-compiler` required
an unconfigured worktree; `autoconf27` succeeded, but `opam exec -- ./configure`
could not complete its C feature probes in this sandbox and ended with
`Neither 32 nor 64 bits architecture`. Consequently both
`make -s boot-compiler` and `make -s test` were attempted but stopped before
execution because `Makefile.build_config` could not be generated. No live
server was restarted or disturbed, and no changes were committed.
