# Builder quick-reference (authoritative spec: the build blueprint §7)

See `docs/plans/2026-07-06-vox-stdlib-build-blueprint.md` for the full
contract. This is the compile recipe only.

```sh
PINNED=/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean
LEAN="${VOX_LEAN:-$(command -v lean 2>/dev/null || echo "$PINNED")}"
OC=/usr/local/home/jujacobs/oxcamls/vox-stdlib/_install/bin/ocamlc.opt
export TMPDIR=/usr/local/home/jujacobs/tmp

# You own vox_stdlib/<Module>.{ml,mli}, vox_stdlib/notes/<Module>.md, and
# vox_stdlib/clients/smoke_<Module>.ml ONLY.
# Verify in a PRIVATE temp dir (avoids the shared VoxCore.olean write race).
# ONE dep-injection style: copy artifacts into the build dir, fail LOUD if a
# declared dependency is missing (never silently compile without it).
B=$(mktemp -d)
cp vox_stdlib/<Module>.mli vox_stdlib/<Module>.ml "$B"/
for dep in <Dep1> <Dep2>; do          # wave 2+ / composition clients; a leaf module has no deps
  # The .cmi filename tracks the dep's SOURCE-file casing (Vlist.cmi but
  # voption.cmi), so resolve case-insensitively: try the module-name casing,
  # then the all-lowercase variant. (Do NOT just lowercase the first letter --
  # that turns Vlist.cmi into the nonexistent vlist.cmi.) The VoxSig olean is
  # ALWAYS capitalized (VoxSig_Voption.olean) -- it derives from the OCaml
  # module name, not the filename.
  cmi="vox_stdlib/_artifacts/$dep.cmi"
  [ -f "$cmi" ] || cmi="vox_stdlib/_artifacts/$(printf %s "$dep" | tr 'A-Z' 'a-z').cmi"
  cp "$cmi" "$B"/                          || { echo "MISSING cmi for $dep"; exit 1; }
  cp "vox_stdlib/_artifacts/VoxSig_$dep.olean" "$B"/ || { echo "MISSING VoxSig_$dep.olean"; exit 1; }
done
( cd "$B"
  $OC -vox-solver-path "$LEAN" -c <Module>.mli    # declares obligations
  $OC -vox-solver-path "$LEAN" -c <Module>.ml )   # seal discharges them
# NB: VoxCore.olean is NOT needed in the build dir -- the compiler regenerates
# it automatically each run (verified). Only the deps' cmi + VoxSig oleans
# need to be present.
```

Notes:

- `.mli` first, then `.ml`. A clean `.ml` = the seal matched every `.mli`
  axiom to a same-named `.ml` theorem.
- **No OCaml `(* *)` comments inside a `[%%vox.lean]` block** — Lean rejects
  `(`; use `--` or `/- -/`. (Real gotcha: the error points at "line N of the
  block", not the comment.)
- **In a block law, dot the exposed-ADT constructor** (`.Vsome x`), never bare.
- Ship `clients/smoke_<Module>.ml`: a few-line goal that forces each shipped
  law to fire (see `scratch_probe/blueprint/wave2/smoke_vset.ml`).
- Do NOT run `make test-one` while iterating (slow). Direct `-c` only.
- Do NOT commit. The integrator commits per wave.
- File a note for every pain-site (blueprint §5), even ones the workaround
  handled cleanly. #32 is hit by bind-then-branch on a spec'd bool, NOT by a
  tail-recursive one-path search — don't file it against the latter.
```
