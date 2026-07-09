(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "adt_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: algebraic data types.  Real verification of ADT
   refinements through Lean's [grind]: every obligation below is
   actually proved (this test fails to compile if any proof fails).
   Exercises constructor introduction, match facts, injectivity,
   distinctness, recursion, bool (Prop) fields, refined constructor
   arguments, cross-module constructor predicates, and negative match
   facts.  No intro or elim forms anywhere: binders bind at their
   skeletons with their refinements as facts, and the annotations
   introduce implicitly.  Note the dead arms: their hypotheses are
   contradictory (the scrutinee's refinement names one constructor,
   the arm's match fact another), so they prove anything -- no runtime
   check, no assumption. *)

type t =
  | K of int
  | L

(* Introduction: the constructor names itself; trivial goal. *)
let k1 : t{ _ = K 1 } = K 1

(* Injectivity via match facts: s = K 3 and s = K y prove y = 3. *)
let get (s : t{ _ = K 3 }) : {r:int | r = 3} =
  match s with
  | K y -> y
  | L -> 0

(* Distinctness: s = L proves s is not K 0. *)
let notk (s : t{ _ = L }) : t{ not (_ = K 0) } = s

(* Recursion, with a wildcard sub-pattern naming a fresh unknown. *)
type ilist =
  | Nil
  | Cons of int * ilist

let head (s : ilist{ _ = Cons (3, Nil) }) : {r:int | r = 3} =
  match s with
  | Cons (h, _) -> h
  | Nil -> 0

(* Bool fields (modelled as Prop): injectivity at Prop works too. *)
type bp = B of bool

let getb (s : bp{ _ = B true }) : {r:bool | r = true} =
  match s with
  | B x -> x

(* Refined constructor arguments compose with match facts: matching
   binds [y] at the skeleton with [y > 0] as a fact. *)
type w =
  | W of {v:int | v > 0}
  | Z

let getw (t : w) : {r:int | r > 0} =
  match t with
  | W y -> y
  | Z -> 1

(* Cross-module: Adt_lib.k3's refinement mentions Adt_lib's
   constructor; the import is matched DIRECTLY, its interned name
   receiving the match facts alongside its .cmi refinement. *)
let three : {r:int | r = 3} =
  match Adt_lib.k3 with
  | Adt_lib.K y -> y
  | Adt_lib.L -> 0

(* Negative match facts, really proved: the default arm knows s is
   neither an A nor B, and exhaustiveness makes it a C. *)
type abc =
  | Ay of int
  | Bee
  | Cee

let classify (s : abc) : {r:int | r >= 0} =
  match s with
  | Ay _ -> 0
  | Bee -> 1
  | _ ->
    let _w : abc{ _ = Cee } = s in
    2
