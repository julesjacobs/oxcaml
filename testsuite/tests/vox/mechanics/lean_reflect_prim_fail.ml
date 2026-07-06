(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: [@@vox.reflect] declarations that must FAIL.  The positive twin
   is demo/lean_reflect_prim.ml.  A reflect binding is TCB (an assumed
   OCaml/Lean correspondence), so the compiler enforces what it CAN --
   payload well-formedness, no double role, no reserved name -- and
   the solver fails closed on an unknown symbol or a false claim. *)

(* An UNKNOWN Lean symbol: the binding is accepted (the compiler does
   not resolve Lean names), but a VC that uses it fails at the solver
   -- fail closed, never a false pass. *)
external b1 : int -> int -> int = "%andint" [@@vox.reflect "no_such_lean_symbol"]
[%%expect{|
external b1 : int -> int -> int = "%andint"
|}]

let uses_unknown (x : int) (m : int) : int{ _ = b1 x m } = b1 x m
[%%expect{|
Line 1, characters 59-65:
1 | let uses_unknown (x : int) (m : int) : int{ _ = b1 x m } = b1 x m
                                                               ^^^^^^
Error: vox: verification failed (lean).
       Goal: no_such_lean_symbol x m = no_such_lean_symbol x m
Hypotheses: <none>
(lean: error: Function expected at)
|}]

(* A block-defined symbol with laws, then a FALSE claim about it: the
   laws entail idempotence, not that a mask is the identity. *)
[%%vox.lean {lean|
opaque band2 : Int -> Int -> Int
@[grind] axiom band2_idem (x : Int) : band2 x x = x
|lean}]
[%%expect{|
|}]

external b2 : int -> int -> int = "%andint" [@@vox.reflect "band2"]
[%%expect{|
external b2 : int -> int -> int = "%andint"
|}]

let false_claim (x : int) (m : int) : int{ _ = x } = b2 x m
[%%expect{|
Line 1, characters 53-59:
1 | let false_claim (x : int) (m : int) : int{ _ = x } = b2 x m
                                                         ^^^^^^
Error: vox: verification failed (lean).
       Goal: band2 x m = x
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* A MALFORMED payload (not a string) is rejected at the declaration. *)
external b3 : int -> int = "%identity" [@@vox.reflect 42]
[%%expect{|
Line 1, characters 39-57:
1 | external b3 : int -> int = "%identity" [@@vox.reflect 42]
                                           ^^^^^^^^^^^^^^^^^^
Error: vox: [@vox.reflect] expects a string payload naming a Lean            symbol, e.g. [@vox.reflect "Int.natAbs"]
|}]

(* A value cannot be BOTH total_ and reflect. *)
let total_ b4 (x : int) = x
[@@vox.reflect "band2"]
[%%expect{|
Lines 1-2, characters 0-23:
1 | let total_ b4 (x : int) = x
2 | [@@vox.reflect "band2"]
Error: vox: a value cannot be both total_ (a translated definition) and [@vox.reflect] (an assumed Lean symbol); choose one
|}]

(* The Vox_/v_ prefixes are reserved for the emitter's own names. *)
external b5 : int -> int = "%identity" [@@vox.reflect "Vox_sneaky"]
[%%expect{|
Line 1, characters 39-67:
1 | external b5 : int -> int = "%identity" [@@vox.reflect "Vox_sneaky"]
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: "Vox_sneaky" may not name a reflected symbol -- the Vox_ and v_          prefixes are reserved for the solver's emitted names
|}]
