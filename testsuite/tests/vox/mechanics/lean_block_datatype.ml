(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Regression: a datatype mentioned ONLY inside a [%%vox.lean] block's
   raw text (never in an OCaml refinement or reflected signature) must
   still be declared to the solver.  Here [b] appears only in [fb]
   inside the block; the sole VC ([g]) mentions [a] via [fa].  Before
   the emitter fix, [b] was never registered on-sight, so [fb]'s
   [.BNil] failed to elaborate ("invalid dotted identifier").  Now
   every [Vox_<name>] a block mentions that resolves to a datatype in
   scope is registered, so both types are declared and the block
   checks. *)

type a =
  | ANil
  | ACons of int * a

type b =
  | BNil
  | BCons of int * b

[%%vox.lean {lean|
@[grind] def fa : Int -> Vox_a -> Int
  | _, .ANil => 0
  | k, .ACons x t => if k = x then 1 else fa k t

@[grind] def fb : Int -> Vox_b -> Int
  | _, .BNil => 0
  | k, .BCons x t => if k = x then 1 else fb k t
|lean}]
[%%expect{|
type a = ANil | ACons of int * a
type b = BNil | BCons of int * b
|}]

(* The only VC mentions [a] (via [fa]); [b] is nowhere in its types. *)
let rec g : (x : a) -> int{ _ = fa 0 x } =
  fun x ->
    match x with
    | ANil -> 0
    | ACons (h, t) -> if 0 = h then 1 else g t
[%%expect{|
val g : (x : a) -> int{ _ = (fa 0 x) } = <fun>
|}]
