(* TEST
 flags = "-vox-solver lean";
 script = "sh ${test_source_directory}/has-lean.sh";
 modules = "adt_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Real verification of ADT refinements through Lean's [grind]: every
   obligation below is actually proved (this test fails to compile if
   any proof fails).  Exercises constructor introduction, match facts,
   injectivity, distinctness, recursion, bool (Prop) fields, refined
   constructor arguments, and cross-module constructor predicates. *)

type t =
  | K of int
  | L

(* Introduction: the constructor names itself; trivial goal. *)
let k1 : t{ _ = K 1 } = refine_ (K 1)

(* Injectivity via match facts: s = K 3 and s = K y prove y = 3. *)
let get (s : t{ _ = K 3 }) : {r:int | r = 3} =
  let refine_ s = s in
  match s with
  | K y -> refine_ y
  | L -> assume_ 0

(* Distinctness: s = L proves s is not K 0. *)
let notk (s : t{ _ = L }) : t{ not (_ = K 0) } =
  let refine_ s' = s in
  refine_ s'

(* Recursion, with a wildcard sub-pattern naming a fresh unknown. *)
type ilist =
  | Nil
  | Cons of int * ilist

let head (s : ilist{ _ = Cons (3, Nil) }) : {r:int | r = 3} =
  let refine_ s = s in
  match s with
  | Cons (h, _) -> refine_ h
  | Nil -> assume_ 0

(* Bool fields (modelled as Prop): injectivity at Prop works too. *)
type bp = B of bool

let getb (s : bp{ _ = B true }) : {r:bool | r = true} =
  let refine_ s = s in
  match s with
  | B x -> refine_ x

(* Refined constructor arguments compose with match facts. *)
type w =
  | W of {v:int | v > 0}
  | Z

let getw (t : w) : {r:int | r > 0} =
  match t with
  | W y -> let refine_ z = y in refine_ z
  | Z -> assume_ 1

(* Cross-module: Adt_lib.k3's refinement mentions Adt_lib's constructor. *)
let three : {r:int | r = 3} =
  let refine_ s = Adt_lib.k3 in
  match s with
  | Adt_lib.K y -> refine_ y
  | Adt_lib.L -> assume_ 0

(* Negative match facts, really proved: the default arm knows s is
   neither an A nor B, and exhaustiveness makes it a C. *)
type abc =
  | Ay of int
  | Bee
  | Cee

let classify (s : abc) : {r:int | r >= 0} =
  match s with
  | Ay _ -> refine_ 0
  | Bee -> refine_ 1
  | _ ->
    let refine_ w = (refine_ s : abc{ _ = Cee }) in
    refine_ 2
