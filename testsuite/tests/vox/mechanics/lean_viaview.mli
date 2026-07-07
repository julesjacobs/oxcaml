(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "lean_viaview.mli lean_viaview.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Task #63 regression (the TRUE guard: fails pre-fix with a universe
   metavariable). An exposed view ADT [vlist_view] has a via-typed field
   [t] (sort [LList]); before the emission-order fix the derived inductive
   was written to the solver BEFORE the block that declares [LList],
   leaving [Sort ?u].

   AUTHORING RULE this test embodies (a single block CANNOT hold both:
   it would have to precede [type vlist_view] to define [LList] the view
   needs, yet follow it to name the view -- an atomic block cannot
   straddle, so the one-block form stays unsealable):
   (1) the ghost-sort-defining block ([inductive LList] + ll ops) comes
       BEFORE [type vlist_view]; (2) the view-referencing model def
       ([vv_depth]) is in a SEPARATE block AFTER it; (3) that view-model
       block is repeated in the .ml so the implementation VCs over the
       view can see it (pending the seal-path splice, task #69). *)

(* Vlist: a verified list behind a via-ABSTRACTED interface.  The
   representation is an ordinary cons-list, but this .mli hides it: [t]
   is [refines (llist)], so a client binds [t] at the Lean list model
   [LList] and reasons purely in list vocabulary (ll_len/ll_mem/ll_app/
   ll_cons/ll_isnil).  The block EXPOSES that model as [public def]s and
   ships the algebra clients need as [public axiom] OBLIGATIONS (the
   default interface-hygiene pattern, blueprint 4): the implementation
   pays each one with a same-named theorem at its seal.  The axioms fire
   ambiently in clients by grind_pattern -- no explicit lemma call.
   House rules: specs mention DEFS (ll_cons), never the LList
   CONSTRUCTORS (.LCons); names carry the [ll_] prefix so they never
   collide with Lean-core identifiers or a co-imported unit's names. *)
type llist [@@vox.sort lean "LList"]
type t : value refines (llist)

[%%vox.lean {lean|
public inductive LList where
  | LNil : LList
  | LCons : Int -> LList -> LList

-- ll_cons is public (clients name it in specs) but NOT [expose]d: it is a
-- non-recursive constructor wrapper, so an exposed body would let grind unfold
-- ll_cons x l => .LCons x l and discharge ll_len_cons / ll_mem_cons WITHOUT the
-- laws, leaving them dead (Phase-C soundness finding).  Kept opaque, those two
-- laws are the only route to ll_len / ll_mem of a cons, so they are LIVE.  The
-- recursive defs below stay [expose]d: unfolding them one step never discharges
-- their inductive laws, so those laws remain live regardless.
@[grind] public def ll_cons (x : Int) (l : LList) : LList := .LCons x l

-- ll_isnil is public but NOT [expose]d, same reasoning as ll_cons: it is
-- non-recursive, so an exposed body would let grind discharge its two facts by
-- reduction and leave them dead.  Kept opaque, they ship as the two laws below
-- (ll_isnil_nil / ll_not_isnil_cons), giving clients an algebra for is_empty's
-- spec vocabulary.
@[grind] public def ll_isnil : LList -> Prop
  | .LNil => True
  | .LCons _ _ => False

-- ll_nil is the empty-list model (opaque, like ll_cons); empty's spec is
-- structural (_ = ll_nil), so ll_isnil_nil is load-bearing at is_empty (empty ()).
@[grind] public def ll_nil : LList := .LNil

@[grind, expose] public def ll_len : LList -> Int
  | .LNil => 0
  | .LCons _ t => 1 + ll_len t

-- ll_head / ll_tail: the destructor vocabulary for the head/tail eliminator.
-- Non-recursive, so opaque (Amendment A) -- their reduction facts ship as
-- ll_head_cons / ll_tail_cons below, which is what makes those laws live.
@[grind] public def ll_head : LList -> Int
  | .LNil => 0
  | .LCons h _ => h

@[grind] public def ll_tail : LList -> LList
  | .LNil => .LNil
  | .LCons _ t => t

@[grind, expose] public def ll_mem (x : Int) : LList -> Prop
  | .LNil => False
  | .LCons y t => x = y ∨ ll_mem x t

@[grind, expose] public def ll_app : LList -> LList -> LList
  | .LNil, m => m
  | .LCons x t, m => .LCons x (ll_app t m)

-- The algebra, shipped as obligations (axiom here, theorem in the .ml).
public axiom ll_isnil_nil : ll_isnil ll_nil
grind_pattern ll_isnil_nil => ll_isnil ll_nil

@[grind] public axiom ll_not_isnil_cons (x : Int) (l : LList) :
    ¬ ll_isnil (ll_cons x l)

public axiom ll_len_nonneg (l : LList) : ll_len l >= 0
grind_pattern ll_len_nonneg => ll_len l

public axiom ll_len_cons (x : Int) (l : LList) :
    ll_len (ll_cons x l) = 1 + ll_len l
grind_pattern ll_len_cons => ll_len (ll_cons x l)

public axiom ll_len_app (a b : LList) :
    ll_len (ll_app a b) = ll_len a + ll_len b
grind_pattern ll_len_app => ll_len (ll_app a b)

public axiom ll_mem_cons (x y : Int) (l : LList) :
    ll_mem x (ll_cons y l) = (x = y ∨ ll_mem x l)
grind_pattern ll_mem_cons => ll_mem x (ll_cons y l)

public axiom ll_mem_app (x : Int) (a b : LList) :
    ll_mem x (ll_app a b) = (ll_mem x a ∨ ll_mem x b)
grind_pattern ll_mem_app => ll_mem x (ll_app a b)

-- Empty is member-free.  ll_nil is opaque, so this is the only route to
-- ll_mem x (empty ()) = False -- the base case for a Mech-A eliminator
-- (Vmap.keys / Vset.elements) that builds a Vlist by recursion.
public axiom ll_nil_not_mem (x : Int) : ¬ ll_mem x ll_nil
grind_pattern ll_nil_not_mem => ll_mem x ll_nil

-- Eliminator algebra (head/tail). ll_head_cons / ll_tail_cons are the reduction
-- facts (live because ll_cons/ll_head/ll_tail are opaque). ll_cons_head_tail is
-- the reconstruction law: on a non-empty list, cons of its head and tail is the
-- list itself -- a client rebuilds `cons (head l) (tail l)` and relates it back
-- to ll_len/ll_mem via the existing ll_len_cons/ll_mem_cons.
public axiom ll_head_cons (x : Int) (l : LList) : ll_head (ll_cons x l) = x
grind_pattern ll_head_cons => ll_head (ll_cons x l)

public axiom ll_tail_cons (x : Int) (l : LList) : ll_tail (ll_cons x l) = l
grind_pattern ll_tail_cons => ll_tail (ll_cons x l)

public axiom ll_cons_head_tail (l : LList) (h : ¬ ll_isnil l) :
    ll_cons (ll_head l) (ll_tail l) = l
grind_pattern ll_cons_head_tail => ll_cons (ll_head l) (ll_tail l)
|lean}]

type vlist_view = VNil | VCons of int * t

[%%vox.lean {lean|
@[grind, expose] public def vv_depth : Vox_Lean_viaview_vlist_view -> Int
  | .VNil => 0
  | .VCons _ _ => 1
|lean}]

val classify : (v : vlist_view) -> int{ _ = vv_depth v }


val empty : (u : unit) -> t{ _ = ll_nil }
val cons : (x : int) -> (l : t) -> t{ _ = ll_cons x l }
val is_empty : (l : t) -> bool{ _ = ll_isnil l }
val length : (l : t) -> int{ _ = ll_len l }
val mem : (x : int) -> (l : t) -> bool{ _ = ll_mem x l }
val append : (a : t) -> (b : t) -> t{ _ = ll_app a b }

(* First-order destructor (Mech B equivalent): guarded head/tail let a client
   traverse structurally -- `if is_empty l then base else recurse (tail l)`.
   The view-ADT form `uncons : t -> (VNil | VCons of int * t)` is blocked by a
   compiler universe bug on via-typed ADT fields (see notes/vlist.md). *)
val head : (l : t) -> int{ _ = ll_head l }
val tail : (l : t) -> t{ _ = ll_tail l }
