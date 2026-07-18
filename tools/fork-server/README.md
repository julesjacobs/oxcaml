# ocamlopt fork-server

A warm fork-server for `ocamlopt.opt`. It attacks the fixed per-process startup
cost of the native compiler — demand-paging the ~74 MB binary, runtime init, and
the eager GC frametable build, ~23 ms of exec+init before any real work — by
paying it **once** and then `fork()`ing a warm child per compile request.

This is a standalone tool. It is **not** wired into dune or any build system;
adoption is entirely opt-in via the drop-in shim below.

## How it works

- `fork_server` (the daemon) execs once and then **never compiles in the
  parent**, so the driver's global mutable state (`Clflags`, `Env`,
  `persistent_env`, …) stays pristine. Each request is served by a `fork()`ed
  child whose text, frametable, and runtime state are already warm via
  copy-on-write. The child re-enters `Optmaindriver.main` with the request's
  argv/cwd/env and exits. This is what makes re-entering the driver safe despite
  its documented "cannot call `main` twice per process" restriction.
- `shim.c` builds to a freestanding `ocamlopt.opt` replacement. Put it first on
  `PATH`; it forwards argv, cwd, environment, and its own stdin/stdout/stderr
  (via `SCM_RIGHTS`) to the daemon, relays the exit code, and **fails open** —
  execs the real compiler — on daemon-absent, hash/OCAMLRUNPARAM/OCAMLLIB
  mismatch, or reply timeout. So it is never worse than stock.

## Recovered cost

~14 ms end-to-end per compile (~15.6 ms with a persistent client connection),
i.e. ~62–71% of the 23 ms exec+init floor. Output is byte-identical. The parent
image is ~51 MB, shared copy-on-write across all children. Because the win is a
fixed per-invocation saving, it concentrates on small compiles; heavy files gain
~0. Farm ceilings (upper bounds, × the true native-compile fraction): ~2–3% of a
full tree, ~10% of a large-app-scale build, ~20% of a core-scale build.

## Safety model

Strictly a **per-user** daemon.

- The socket lives in a `0700` per-user runtime dir and is `chmod`ed `0600`;
  every accepted connection is checked with `SO_PEERCRED` and dropped unless the
  peer uid equals the daemon's — before any parsing or fork. The compiler-hash
  is a **version compatibility check, not authentication**.
- Init-time-cached configuration is guarded by a handshake: `OCAMLRUNPARAM` (GC
  params, fixed at daemon startup) and `OCAMLLIB` (stdlib path, cached in
  `Config.standard_library` at process init). A request whose value differs is
  refused so the shim fails open rather than compiling with the wrong config.
  **Rule for maintainers:** any env var the compiler caches at process-init
  rather than reading per-invocation must be added to this handshake, like
  `OCAMLLIB`/`OCAMLRUNPARAM`; per-request `clearenv` cannot fix an
  already-cached value. `PATH` is exempt (read fresh when the child spawns
  `as`). When extending, a quick grep of `Config`/`Clflags` init for `getenv`
  is recommended; and a full byte-identity run with production config (the
  self-build compiled 1171/1171 modules byte-identically through the daemon)
  will surface in practice any init-time var that bites.
- Requests are length-framed with a 4 MB cap and a per-request wall-clock
  deadline; received fds are closed on every error path; the accept loop
  survives any transient error, `SIGPIPE`, and child crashes; `SIGTERM` unlinks
  the socket cleanly.

Known bound: the acceptor is single-threaded, so a same-uid client can stall the
loop up to the per-request deadline (~5 s) per connection. Accepted because a
same-uid actor already has stronger self-DoS, and cooperative clients send their
request atomically. Full removal would need an async/read-in-child acceptor.

## Usage (A/B example)

    # 1. one daemon per (compiler, OCAMLRUNPARAM, OCAMLLIB)
    OCAMLRUNPARAM=o=120 fork_server /run/user/$UID/oxfork.sock 2>daemon.err &
    HASH=$(grep -oE 'hash=[0-9a-f]+' daemon.err | cut -d= -f2)

    # 2. point PATH at a dir whose ocamlopt.opt is the shim, and export:
    export OXFORK_REAL=/path/to/real/ocamlopt.opt
    export OXFORK_SOCK=/run/user/$UID/oxfork.sock
    export OXFORK_HASH=$HASH            # "" to skip the version check
    # optional: OXFORK_TIMEOUT (reply timeout s, default 300), OXFORK_LOG

Only `-c` compiles are delegated; link/pack/`-config`/`-version` and any failure
exec the real compiler. Install note: a shim installed *as* `ocamlopt.opt` must
forward `argv[0]` (`shim` already sends a normalized `argv[0]`), because the
driver treats `args[0]` as the program name and skips it.

## Pre-deploy checklist

- Verify cross-uid connections are **rejected** with one real second-account
  connection attempt in CI (the `SO_PEERCRED` gate is code-verified and
  same-uid-tested, but genuine cross-uid rejection has not been exercised live).
- Key one daemon per compiler build hash; a rebuilt compiler replies `HASHMISS`
  and the shim falls back until a fresh daemon for the new hash is started.
- Clean the child's `TMPDIR` on kill (a `SIGKILL`ed child can leave the
  assembler's temporary `.s` file behind).
- Accept (or address) the known same-uid bound: the single-threaded acceptor
  lets a same-uid client stall the loop up to the per-request deadline (~5 s)
  per connection. Mitigated by the deadline; full removal needs an
  async/read-in-child acceptor. Not a cross-uid risk (the uid gate blocks that).
