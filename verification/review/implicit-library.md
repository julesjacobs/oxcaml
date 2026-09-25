# Implicit refinements in the shared library

Remove 599 obsolete `refine_` occurrences from 16 shared source/interface
files: Borrow, Borrow_iarray, Raw_memory, Vox_control, Vox_iarray,
Vox_int_sequence, Vox_traversal, Vox_sequence, Pref and Ghost_pref.
Refinement predicates, primitive declarations, ownership modes and proof calls
are retained. Unit-result aliases and unused proof names are simplified.
Ordinary predicate bindings remain where needed to preserve matching callback
signatures. Dependent clients that duplicate those signatures must migrate the
same bindings together.

The existing compiler introduces refinements from expected types and exposes
refinements on expression use (`typing/typecore.ml`, `introduce_refinement`,
`eliminate_refinement` and `type_expect`). No compiler/parser change or new
assumption is involved. Tuple construction may still need an expected type.

Base: `217375e8831298f6ca52c46756ae4f98ae9f8890` (PR #205).
The read-only compiler installation is `worktrees/time-credits/_install`,
source HEAD `9d5d8fca7a3261a3b06c293fe7aa30a38b9a2ef0`;
`typecore.ml` blob `41e44de7b3ffb8d2e07c6aa360f3226fcc296d20` and
`vox_vc.ml` blob `c5589830f72843e364031f16720c05174f1122f1`.
Installed SHA256:

- ocamlc: `2d96a631b25d477a0cf3c265fdbd020eb30742756d9aa1115a38f85256a0df9d`
- ocamlopt: `f8953e9990f0c62c3b7bdab99e7e8f6051c2e9c3ac7eefba980801a2a06d2bb0`

Run `python3 verification/review/check_implicit_library.py COMPILER_PREFIX`.
This compiles every library interface/implementation in the existing library
build order on both backends, including unchanged downstream table/ownership
modules, with isolated output in `_build/refine-library`. It never installs or
modifies the supplied compiler. The complete run passed. Borrow demo, ranges,
parallel, and Pref ownership execution regressions also passed using ocamltest
with installed compiler overrides. Cost and structure clients/rejections and
erasure are covered by their existing boundary scripts in the follow-up.

The unrelated `pref_records` callback-payload test fails VC translation on this
compiler both before and after migration, including when its original Pref
interface and implementation are rebuilt. It is not counted as passing.
Explicit refinement parser/typing/negative compatibility fixtures are preserved.
