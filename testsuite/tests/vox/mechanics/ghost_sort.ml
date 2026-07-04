(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* GHOST SORTS (solverless coverage): the eager validation of
   [@@vox.sort lean "Name"] and the VC shape a ghost-sorted binder
   produces, without needing a Lean binary.  The proofs and
   counterexample are in lean_ghost_sort.ml.  See DESIGN.md. *)

[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind] def mem (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind] def ins (x : Int) (s : ISet) : ISet := ISet.cons x s
|lean}]
[%%expect{|
|}]

(* A ghost sort naming a block-defined Lean type is accepted; its
   values are declared at [ISet], so API refinements speak the set
   vocabulary directly instead of the underlying representation. *)
module S : sig
  type iset [@@vox.sort lean "ISet"]

  val add : (x : int) -> (s : iset) -> iset{ _ = ins x s } @ unique
  val mem : (x : int) -> (s : iset) -> bool{ _ = mem x s }
end = struct
  type iset = { mutable c : int } [@@vox.sort lean "ISet"]

  let add : (x : int) -> (s : iset) -> iset{ _ = ins x s } @ unique =
    fun x s -> ignore (x, s); assume_unchecked_ (Obj.magic_unique { c = 0 })

  let mem : (x : int) -> (s : iset) -> bool{ _ = mem x s } =
    fun x s -> ignore (x, s); assume_unchecked_ false
end
[%%expect{|
Line 10, characters 48-76: vox VC (ASSUMED):
  goal: *unknown1* = (ins x s)
  hypotheses: <none>
Line 13, characters 48-53: vox VC (ASSUMED):
  goal: false = (mem x s)
  hypotheses: <none>
module S :
  sig
    type iset
    val add : (x : int) -> (s : iset) -> iset{ _ = (ins x s) } @ unique
    val mem : (x : int) -> (s : iset) -> bool{ _ = (mem x s) }
  end
|}]

(* A client VC: the ghost-sort binder [t] carries [t = ins x s] and
   flows into the goal as an ISet value (a spec-function argument), not
   a VoxU opaque. *)
let use : (x : int) -> (s : S.iset) -> bool{ _ = mem x (ins x s) } =
  fun x s ->
    let t = S.add x s in
    S.mem x t
[%%expect{|
Line 4, characters 4-13: vox VC:
  goal: *unknown3* = (mem x (ins x s))
  hypotheses:
  *unknown3* = (mem x t)
  t = (ins x s)
val use : (x : int) -> (s : S.iset) -> bool{ _ = (mem x (ins x s)) } = <fun>
|}]

(* The ghost sort is also reachable through the refines KIND spelling by
   PATH: [refines (S.iset)] resolves the OCaml type path to the same
   Lean sort via the existing refines-kind elaboration, so an abstract
   type may be DECLARED at a ghost sort in its kind -- what a client
   binding an abstract [t : value refines (iset)] in an .mli relies on. *)
type via_kind : value refines (S.iset)
[%%expect{|
type via_kind
|}]

let via_kind_use (x : via_kind{ _ = ins 1 x }) =
  let refine_ ok = (x : via_kind{ _ = ins 1 x }) in
  ok
[%%expect{|
Line 2, characters 20-21: vox VC:
  goal: x = (ins 1 x)
  hypotheses:
  x = (ins 1 x)
val via_kind_use : via_kind{ _ = (ins 1 _) } -> via_kind = <fun>
|}]

(* Eager validation of malformed declarations, mirroring
   lean_vox_sort.ml. *)

(* [lean] with no name: malformed payload shape. *)
type t1 [@@vox.sort lean]
[%%expect{|
Line 1, characters 8-25:
1 | type t1 [@@vox.sort lean]
            ^^^^^^^^^^^^^^^^^
Error: vox: unknown vox.sort "lean" (expected "int", "bool", "opaque", or lean "Name")
|}]

(* An empty Lean name. *)
type t2 [@@vox.sort lean ""]
[%%expect{|
Line 1, characters 8-28:
1 | type t2 [@@vox.sort lean ""]
            ^^^^^^^^^^^^^^^^^^^^
Error: vox: vox.sort lean requires a non-empty Lean type name
|}]

(* A non-identifier name (a Lean application, not a single type name):
   rejected eagerly rather than rendered verbatim into broken solver
   input. *)
type t3 [@@vox.sort lean "Set Int"]
[%%expect{|
Line 1, characters 8-35:
1 | type t3 [@@vox.sort lean "Set Int"]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: "Set Int" is not a valid Lean type name for vox.sort lean
|}]

(* On a pure ALIAS the attribute is ignored by sorting (aliases expand
   first), so it is rejected -- exactly as for [int]/[bool]. *)
type t4 = int [@@vox.sort lean "ISet"]
[%%expect{|
Line 1, characters 14-38:
1 | type t4 = int [@@vox.sort lean "ISet"]
                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: vox.sort on a type alias has no effect (an alias expands to its definition before sorting); put the attribute on the definition
|}]

(* The scalar spelling's message now also mentions the lean form. *)
type t5 [@@vox.sort float]
[%%expect{|
Line 1, characters 8-26:
1 | type t5 [@@vox.sort float]
            ^^^^^^^^^^^^^^^^^^
Error: vox: unknown vox.sort "float" (expected "int", "bool", "opaque", or lean "Name")
|}]
