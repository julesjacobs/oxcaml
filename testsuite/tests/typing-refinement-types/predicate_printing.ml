(* TEST
 readonly_files = "predicate_printing_loaded.mli";
 setup-ocamlc.byte-build-env;
 {
   flags = "-extension refinement_types -i";
   compiler_output = "inferred.mli";
   ocamlc.byte;
   flags = "-extension refinement_types -i";
   compiler_output = "compile.output";
   module = "inferred.mli";
   ocamlc.byte;
   script = "cmp inferred.mli compile.output";
   script;
   flags = "-extension refinement_types";
   ocamlc.byte;
   module = "predicate_printing_loaded.mli";
   flags = "-extension refinement_types -i";
   compiler_output = "loaded.mli";
   ocamlc.byte;
   module = "loaded.mli";
   flags = "-extension refinement_types";
   compiler_output = "loaded.output";
   ocamlc.byte;
 }
 {
   flags = "-extension refinement_types -principal -i";
   compiler_output = "principal.mli";
   ocamlc.byte;
   flags = "-extension refinement_types -principal -i";
   compiler_output = "principal.output";
   module = "principal.mli";
   ocamlc.byte;
   script = "cmp principal.mli principal.output";
   script;
 }
*)

type t = { x : int | let f (y : int) = y = y in f x }
type b = { x : bool | let f (y : bool) = y = y in f x }
type scalar = int
type alias = { x : scalar | let f (y : scalar) = y = y in f x }
type local = { x : int | let y : int = x in y = y }
type matched =
  { x : int option | match x with None -> true | Some y -> y = y }
type 'a independent =
  { x : 'a | let id y = y in let f (y : int) = y = y in id (f 0) }

type function_annotation =
  { x : int | let eq = ((=) : int -> int -> bool) in eq x x }
type tuple_annotation =
  { x : int * bool | let f (y : int * bool) = match y with (n, b) -> n = n in f x }
type expression_annotation =
  { x : int | match (x : int) with y -> y = y }
type 'a shared =
  { x : 'a | let id (y : 'a) = y in let _result = id x in true }
type 'a named_sharing =
  { x : 'a | let id (y : 'b) = y in let _result = id x in id true }
module Named = struct type t = int end
type named =
  { x : Named.t | let f (y : Named.t) = y = y in f x }
module F (M : sig type t = int end) = struct
  type t = { x : M.t | let f (y : M.t) = y = y in f x }
end
module Applied = F (Named)
type 'a wildcard =
  { x : 'a | let id (y : _) = y in let _result = id x in id true }
type 'a wildcard_list =
  { x : 'a list | let id (y : _ list) = y in
    let _result = id x in let _bools = id [true] in true }
type pattern_annotation =
  { x : int option | match x with None -> true | Some (y : int) -> y = y }
type nested_annotation =
  { x : { n : int | n = n } |
    let f (y : { n : int | n = n }) = let refine_ z = y in z = z in f x }
