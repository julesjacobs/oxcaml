(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* GHOST SORTS, generalized: [@@vox.sort lean "Name"] lets an OCaml
   type declare that its LOGICAL REPRESENTATIVE is a block-defined Lean
   type -- not just the base sorts [int]/[bool] (lean_vox_sort.ml) but
   any type the module's [%%vox.lean] blocks define.  The OCaml type is
   a pure phantom (its values are opaque handles); the Lean name is
   what its binders are declared at in the solver, so API refinements
   are stated directly in the Lean vocabulary of that type.  TRUSTED:
   the declaring library asserts every fact it issues is true of that
   interpretation (same trust class as [@@vox.sort int]).  See
   DESIGN.md. *)

(* A finite-set model in core Lean (no Mathlib): an inductive list of
   elements (duplicates are irrelevant to membership).  [ISet] is what
   [iset] binders are declared at; [ins]/[uni]/[mem]/[card] are the
   API's spec functions. *)
[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind] def mem (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind] def ins (x : Int) (s : ISet) : ISet := ISet.cons x s

@[grind] def uni : ISet -> ISet -> ISet
  | .nil, t => t
  | .cons x s, t => ISet.cons x (uni s t)

@[grind] def card : ISet -> Int
  | .nil => 0
  | .cons _ s => 1 + card s
|lean}]

module S : sig
  type iset [@@vox.sort lean "ISet"]

  val add : (x : int) -> (s : iset) -> iset{ _ = ins x s } @ unique
  val union : (s : iset) -> (t : iset) -> iset{ _ = uni s t } @ unique
  val mem : (x : int) -> (s : iset) -> bool{ _ = mem x s }
  val card : (s : iset) -> int{ _ = card s }
end = struct
  (* The runtime representation is irrelevant; a dummy cell suffices,
     and every fact is asserted (the library's axiom). *)
  type iset = { mutable c : int } [@@vox.sort lean "ISet"]

  let add : (x : int) -> (s : iset) -> iset{ _ = ins x s } @ unique =
    fun x s -> ignore (x, s); assume_unchecked_ (Obj.magic_unique { c = 0 })

  let union : (s : iset) -> (t : iset) -> iset{ _ = uni s t } @ unique =
    fun s t -> ignore (s, t); assume_unchecked_ (Obj.magic_unique { c = 0 })

  let mem : (x : int) -> (s : iset) -> bool{ _ = mem x s } =
    fun x s -> ignore (x, s); assume_unchecked_ false

  let card : (s : iset) -> int{ _ = card s } =
    fun s -> ignore s; assume_unchecked_ 0
end
[%%expect{|
module S :
  sig
    type iset
    val add : (x : int) -> (s : iset) -> iset{ _ = ins x s } @ unique
    val union : (s : iset) -> (t : iset) -> iset{ _ = uni s t } @ unique
    val mem : (x : int) -> (s : iset) -> bool{ _ = mem x s }
    val card : (s : iset) -> int{ _ = card s }
  end
|}]

(* A client proves THROUGH the ghost sort: [t] is an [iset] binder,
   declared at [ISet], carrying [t = ins x s]; [S.mem x t] gives
   [result = mem x t], and congruence closes the goal.  The client
   never sees the representation -- only the Lean set vocabulary. *)
let roundtrip : (x : int) -> (s : S.iset) -> bool{ _ = mem x (ins x s) } =
  fun x s ->
    let t = S.add x s in
    S.mem x t
[%%expect{|
val roundtrip : (x : int) -> (s : S.iset) -> bool{ _ = mem x (ins x s) } =
  <fun>
|}]

(* Composition through two ghost-sort binders. *)
let via_union : (x : int) -> (s : S.iset) -> (t : S.iset)
  -> bool{ _ = mem x (uni s t) } =
  fun x s t ->
    let u = S.union s t in
    S.mem x u
[%%expect{|
val via_union :
  (x : int) -> (s : S.iset) -> (t : S.iset) -> bool{ _ = mem x (uni s t) } =
  <fun>
|}]

(* An arithmetic fact ABOUT the abstract set: [card] exposes an Int
   observable, and [card (ins x s) = 1 + card s] is proved by the
   library's model (unfolding [ins]/[card]). *)
let grows : (x : int) -> (s : S.iset) -> int{ _ = card s + 1 } =
  fun x s ->
    let t = S.add x s in
    S.card t
[%%expect{|
val grows : int -> (s : S.iset) -> int{ _ = card s + 1 } = <fun>
|}]

(* An OVERCLAIM: adding one element grows the cardinality by exactly
   one, not by [x].  The goal reduces to [x = 1] arithmetically, so the
   solver rejects it and prints a concrete counterexample. *)
let overclaim : (x : int) -> (s : S.iset) -> int{ _ = card s + x } =
  fun x s ->
    let t = S.add x s in
    S.card t
[%%expect{|
Line 4, characters 4-12:
4 |     S.card t
        ^^^^^^^^
Error: vox: verification failed (lean).
       Goal: *unknown13* = card s + x
Hypotheses:
  *unknown13* = card t
  t = ins x s
Possible counterexample:
  x = 0
  *unknown13* = 1
  card s = 0
  card t = 1
  card (ISet.cons x s) = 1
(lean: error: `grind` failed)
|}]

(* The sort is logic-level only: the program cannot treat [iset] as its
   representation. *)
let bad (x : S.iset) = x + 1
[%%expect{|
Line 1, characters 23-24:
1 | let bad (x : S.iset) = x + 1
                           ^
Error: The value "x" has type "S.iset" but an expression was expected of type "int"
|}]
