(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: dependent arrows (DESIGN.md's end-to-end example).  [lt]'s
   result refinement mentions its parameters; applying it to variables
   substitutes their stamps, so the unpacked [c] carries [c = (z < x)]
   and the path fact discharges [div]'s precondition. *)

let zero : {v:int | v = 0} = assume_ 0
[%%expect{|
Line 1, characters 37-38: vox VC (RUNTIME CHECKED):
  goal: 0 = 0
  hypotheses: <none>
val zero : int{ _ = 0 } = 0
|}]

let lt : (x : int) -> (y : int) -> {z:bool | z = (x < y)} =
  fun x y -> assume_ (x < y)
[%%expect{|
Line 2, characters 21-28: vox VC (RUNTIME CHECKED):
  goal: (x < y) = (x < y)
  hypotheses:
  zero = 0
  zero = 0
val lt : (x : int) -> (y : int) -> bool{ _ = (x < y) } = <fun>
|}]

let div (a : int) (b : {v:int | not (v = 0)}) : int =
  a / b
[%%expect{|
val div : int -> int{ not (_ = 0) } -> int = <fun>
|}]

let safe (x : int) : int =
  let refine_ z = zero in
  let refine_ c = lt z x in
  if c then div 100 (refine_ x) else 0
[%%expect{|
Line 4, characters 29-30: vox VC:
  goal: not (x = 0)
  hypotheses:
  c
  c = (z < x)
  z = zero
  z = 0
  zero = 0
  zero = 0
val safe : int -> int = <fun>
|}]

(* Partial application: indices need no renumbering. *)
let partial (a : int) (b : int) : {w:bool | w || not w} =
  let lta = lt a in
  let refine_ c = lta b in
  refine_ (c || not c)
[%%expect{|
Line 4, characters 10-22: vox VC:
  goal: (c || (not c)) || (not (c || (not c)))
  hypotheses:
  c = (a < b)
  zero = 0
  zero = 0
val partial : int -> int -> bool{ _ || (not _) } = <fun>
|}]

(* Dependent LATER PARAMETERS: the binder is opened at the lambda
   (checking [p] against [{v | v = x}] instantiated at this activation's
   [x]) and instantiated at each application (the caller must supply a
   proof about ITS argument). *)
let apply : (x:int) -> {v:int | v = x} -> int =
  fun x p -> (p :> int)
[%%expect{|
val apply : (x : int) -> int{ _ = x } -> int = <fun>
|}]

(* Contract parameters: arguments are passed BARE and the predicate is
   discharged at the argument's logical name; literals name
   themselves, for dependent parameters too. *)
let use_apply (a : int) : int = apply a a
[%%expect{|
Line 1, characters 40-41: vox VC:
  goal: a = a
  hypotheses:
  zero = 0
  zero = 0
val use_apply : int -> int = <fun>
|}]

let use_apply_lit : int = apply 5 5
[%%expect{|
Line 1, characters 34-35: vox VC:
  goal: 5 = 5
  hypotheses:
  zero = 0
  zero = 0
val use_apply_lit : int = 5
|}]

(* Recursion is sound by construction: the recursive call
   re-instantiates [prev]'s type at [x'], so only a closure refined at
   the NEW argument is accepted, and the unpacked fact inside each
   activation is about that activation's own [x]. *)
let rec countdown : (x:int) -> (unit -> {v:int | v = x}) option -> int =
  fun x prev ->
    (match prev with
     | Some g ->
       let refine_ r = g () in
       let _w : {v:int | v = x} = refine_ r in
       ()
     | None -> ());
    if x = 0 then 0
    else
      let x' = x - 1 in
      let f : unit -> {v:int | v = x'} = fun () -> assume_ x' in
      countdown x' (Some f)
[%%expect{|
Line 6, characters 42-43: vox VC:
  goal: r = x
  hypotheses:
  r = x
  zero = 0
  zero = 0
Line 12, characters 59-61: vox VC (RUNTIME CHECKED):
  goal: x' = x'
  hypotheses:
  x' = (x - 1)
  not (x = 0)
  zero = 0
  zero = 0
val countdown : (x : int) -> (unit -> int{ _ = x }) option -> int = <fun>
|}]

(* Coercion between dependent arrow types goes through the binder
   pairing, so the identity (and alpha-renamed) coercion works. *)
let dep_coerce = (apply :> (y:int) -> {v:int | v = y} -> int)
[%%expect{|
val dep_coerce : (y : int) -> int{ _ = y } -> int = <fun>
|}]
