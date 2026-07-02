(* TEST
 flags = "-dump-vc -vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: dependent arrows (DESIGN.md's end-to-end example).  [lt]'s
   result refinement mentions its parameters; applying it to variables
   substitutes their stamps, so the unpacked [c] carries [c = (z < x)]
   and the path fact discharges [div]'s precondition. *)

let zero : {v:int | v = 0} = assume_ 0

let lt : (x : int) -> (y : int) -> {z:bool | z = (x < y)} =
  fun x y -> assume_ (x < y)

let div (a : int) (b : {v:int | not (v = 0)}) : int =
  a / (let refine_ b = b in b)

let safe (x : int) : int =
  let refine_ z = zero in
  let refine_ c = lt z x in
  if c then div 100 (refine_ x) else 0

(* Partial application: indices need no renumbering. *)
let partial (a : int) (b : int) : {w:bool | w || not w} =
  let lta = lt a in
  let refine_ c = lta b in
  refine_ (c || not c)
