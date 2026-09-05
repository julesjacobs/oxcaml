(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -i";
 compiler_output = "inferred.mli";
 ocamlc.byte;
 flags = "-extension refinement_types";
 compiler_output = "compile.output";
 module = "inferred.mli";
 ocamlc.byte;
 script = "cp ${test_source_directory}/dependent_printing.ml inferred.ml";
 script;
 module = "inferred.ml";
 ocamlc.byte;
*)

external f : (x:int) -> (y:int) -> {r:int | r = x + y} = "f"
let y = 1
let partial = f y
let check : (z:int) -> {r:int | r = y + z} = partial

type tuple = (p : (int * int)) -> {r:int | match p with a,b -> r = a+b}
type nested = (x:int) -> (x: {r:int | r=x}) -> {r:int | let refine_ n=x in r=n}
type predicate = (x:int) -> {r:int | let x = x + 1 in r=x}

let g (x:int) : {r:int | r=x} = refine_ x
module M = struct
  let x = 1
  let y = g x
end
let exported : {r:int | r=M.x} = M.y
module Copy = M
let copied : {r:int | r=Copy.x} = Copy.y
module F (X : sig val x : int end) = struct
  let x = X.x
  let y = g x
end
module Applied = F (M)
let applied : {r:int | r=Applied.x} = Applied.y

open M
type qualified = (x:int) -> {r:int | r=x+M.x}
