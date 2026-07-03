(* TEST
 flags = "-vox-solver z3";
 script = "sh ${test_source_directory}/../has-z3.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: the z3 backend proves what the lean demos prove -- ints,
   dependent arrows, datatypes (declare-datatypes), records, match
   facts, and negative match facts via native testers.  This test
   fails to compile if any obligation fails; the must-fail twins are
   z3_fail.ml and z3_adt_fail.ml. *)

let div (a : int) (b : {v:int | not (v = 0)}) : int =
  let refine_ b = b in
  a / b

let safe (x : int) = if 0 < x then div 100 (refine_ x) else 0

let mul : (x : int) -> (y : int) -> {z:int | z = x * y} =
  fun x y -> refine_ (x * y)

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
  | L -> refine_ 0

type point =
  { px : int
  ; py : int
  }

let swap : (p : point) -> point{ _.px = p.py && _.py = p.px } =
  fun p -> refine_ { px = p.py; py = p.px }

(* Negative match facts through z3's native testers: the default arm
   knows s is neither an Ay nor Bee, so it is a Cee. *)
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
