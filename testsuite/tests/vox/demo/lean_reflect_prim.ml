(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Demo: EXTENSIBLE reflection primitives.  [@@vox.reflect "sym"] binds
   an OCaml value's solver-side name to a Lean symbol, entering the
   reflection table for BOTH code reflection (calls in checked
   positions and reflected bodies) and predicate translation (the name
   used in a refinement).  Unlike [total_], no definition is emitted --
   the symbol must already exist; here a [%%vox.lean] block supplies it.

   This is impossible with the built-in table alone: bit operations are
   externals, so today they hit no primitive rule and become opaque
   atoms (nothing provable), and they cannot be [total_]'d (no OCaml
   body to translate).  The declaration is TCB: the OCaml primitive
   [%andint] and the Lean [bland] are linked only by the author's word,
   exactly like a [%%vox.lean] axiom or a [@@vox.sort lean "Name"] on a
   type.  The block's laws are likewise assumed here (a faithful Int
   bitwise model is out of scope); the point is the BINDING mechanism. *)

[%%vox.lean {lean|
opaque bland : Int -> Int -> Int
@[grind] axiom bland_idem  (x : Int)     : bland x x = x
@[grind] axiom bland_zero  (x : Int)     : bland x 0 = 0
@[grind] axiom bland_comm  (x y : Int)   : bland x y = bland y x
@[grind] axiom bland_assoc (x y z : Int) : bland (bland x y) z = bland x (bland y z)
|lean}]

(* The OCaml bitwise-and, declared to denote [bland]. *)
external band : int -> int -> int = "%andint" [@@vox.reflect "bland"]

(* Masking with the same mask twice is idempotent: (x & m) & m = x & m.
   The refinement spells the OCaml name [band]; it reflects to [bland],
   the SAME symbol the checked expression does, so the VC is
   [bland (bland x m) m = bland x m], closed by grind from assoc+idem. *)
let mask_idem (x : int) (m : int) : int{ _ = band x m } =
  band (band x m) m

(* Two masks commute: (x & a) & b = (x & b) & a. *)
let mask_commute (x : int) (a : int) (b : int)
  : int{ _ = band (band x b) a } =
  band (band x a) b

(* Masking a masked value by zero is zero (uses bland_zero, and the
   nested application still reflects). *)
let mask_then_zero (x : int) (m : int) : int{ _ = 0 } =
  band (band x m) 0
