(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* This file tests that mode/modality annotations are properly propogated from
   the parsetree into the typedtree. It does so by typing a bit of code that
   contains an `@ portable`/`@@ portable` annotation, then iterating over the
   typedtree and asserting that `@ portable`/`@@ portable` appears somewhere in
   it. *)

(********** Setup **********)

let run s =
  (* Parse and typecheck the string as an impl. *)
  let pi = Parse.implementation (Lexing.from_string s) in
  let pm = Ast_helper.Mod.structure pi in
  let tm, _ = Typemod.type_module (Lazy.force Env.initial) pm in
  (* Check if the typedtree has a "@ portable" annotation anywhere in it. *)
  let has_portable_annotation = ref false in
  let modes iterator (modes : _ Typedtree.modes) =
    List.iter
      (fun (mode : Mode.Alloc.atom Location.loc) ->
        match mode.txt with
        | Atom (Comonadic Portability, Portable) ->
          has_portable_annotation := true
        | _ -> ())
      modes.mode_desc;
    Tast_iterator.default_iterator.modes iterator modes
  in
  let modalities iterator (modalities : Typedtree.modalities) =
    List.iter
      (fun (mode : Mode.Modality.atom Location.loc) ->
        match mode.txt with
        | Atom (Comonadic Portability, Meet_const Portable) ->
          has_portable_annotation := true
        | _ -> ())
      modalities.moda_desc;
    Tast_iterator.default_iterator.modalities iterator modalities
  in
  let iterator = { Tast_iterator.default_iterator with modes; modalities } in
  iterator.module_expr iterator tm;
  if !has_portable_annotation
  then Format.printf "Has annotation@."
  else failwith "Error: annotation missing"
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

let run_exact_modality expected s =
  let expected_name =
    match expected with
    | `Total -> "total/Total"
    | `Partial -> "partial/Partial"
    | `Logical -> "logical/Logical"
    | `Nonlogical -> "nonlogical/Physical"
  in
  let pi = Parse.implementation (Lexing.from_string s) in
  let pm = Ast_helper.Mod.structure pi in
  let tm, _ = Typemod.type_module (Lazy.force Env.initial) pm in
  let found = ref false in
  let modalities iterator (modalities : Typedtree.modalities) =
    List.iter
      (fun (modality : Mode.Modality.atom Location.loc) ->
        let matches =
          match[@warning "-4"] expected, modality.txt with
          | `Total, Atom (Comonadic Totality, Meet_const Total)
          | `Partial, Atom (Comonadic Totality, Meet_const Partial)
          | `Logical, Atom (Monadic Logicality, Join_const Logical)
          | `Nonlogical, Atom (Monadic Logicality, Join_const Physical) ->
            true
          | _ -> false
        in
        found := !found || matches)
      modalities.moda_desc;
    Tast_iterator.default_iterator.modalities iterator modalities
  in
  let iterator = { Tast_iterator.default_iterator with modalities } in
  iterator.module_expr iterator tm;
  if !found
  then Format.printf "Has exact %s modality@." expected_name
  else failwith "Error: exact modality missing"
;;

[%%expect{|
val run_exact_modality :
  [ `Logical | `Nonlogical | `Partial | `Total ] -> string -> unit = <fun>
|}];;

let run_exact_structure_primitive_mode expected s =
  let expected_name =
    match expected with
    | `Total -> "total/Total"
    | `Partial -> "partial/Partial"
    | `Logical -> "logical/Logical"
    | `Nonlogical -> "nonlogical/Physical"
  in
  let pi = Parse.implementation (Lexing.from_string s) in
  let pm = Ast_helper.Mod.structure pi in
  let tm, _ = Typemod.type_module (Lazy.force Env.initial) pm in
  let found = ref false in
  let modes iterator (modes : _ Typedtree.modes) =
    List.iter
      (fun (mode : Mode.Alloc.atom Location.loc) ->
        let matches =
          match[@warning "-4"] expected, mode.txt with
          | `Total, Atom (Comonadic Totality, Total)
          | `Partial, Atom (Comonadic Totality, Partial)
          | `Logical, Atom (Monadic Logicality, Logical)
          | `Nonlogical, Atom (Monadic Logicality, Physical) -> true
          | _ -> false
        in
        found := !found || matches)
      modes.mode_desc;
    Tast_iterator.default_iterator.modes iterator modes
  in
  let iterator = { Tast_iterator.default_iterator with modes } in
  iterator.module_expr iterator tm;
  if !found
  then Format.printf "Has exact %s structure primitive mode@." expected_name
  else failwith "Error: exact structure primitive mode missing"
;;

[%%expect{|
val run_exact_structure_primitive_mode :
  [ `Logical | `Nonlogical | `Partial | `Total ] -> string -> unit = <fun>
|}];;

(* Validate testing strategy but checking that we get an error message if no
   modes/modalities appear *)
run {| type t |};;

[%%expect{|
Exception: Failure "Error: annotation missing".
|}];;

(********** Tests **********)

(* These tests inspect the exact internal modality atoms, rather than merely
   checking that the new spellings parse.  Test both value declarations and
   record fields, the two principal declaration sites for modalities. *)

run_exact_modality `Total
  {| module type S = sig val f : int -> int @@ total end |};;
run_exact_modality `Partial
  {|
    [@@@warning "-220"]
    module type S = sig val f : int -> int @@ partial end
  |};;
run_exact_modality `Logical
  {| module type S = sig val f : int -> int @@ logical end |};;
run_exact_modality `Nonlogical
  {|
    [@@@warning "-220"]
    module type S = sig val f : int -> int @@ nonlogical end
  |};;

[%%expect{|
Has exact total/Total modality
- : unit = ()
Has exact partial/Partial modality
- : unit = ()
Has exact logical/Logical modality
- : unit = ()
Has exact nonlogical/Physical modality
- : unit = ()
|}];;

run_exact_modality `Total {| type t = { f : (int -> int) @@ total } |};;
run_exact_modality `Partial
  {| [@@@warning "-220"] type t = { f : (int -> int) @@ partial } |};;
run_exact_modality `Logical {| type t = { f : (int -> int) @@ logical } |};;
run_exact_modality `Nonlogical
  {| [@@@warning "-220"] type t = { f : (int -> int) @@ nonlogical } |};;

[%%expect{|
Has exact total/Total modality
- : unit = ()
Has exact partial/Partial modality
- : unit = ()
Has exact logical/Logical modality
- : unit = ()
Has exact nonlogical/Physical modality
- : unit = ()
|}];;

(* Structure primitives convert their syntactic modality lists to value modes.
   Each modality spelling denotes its corresponding value-mode atom. *)
run_exact_structure_primitive_mode `Total
  {| external f_total : int -> int @@ total = "%identity" |};;
run_exact_structure_primitive_mode `Partial
  {| external f_partial : int -> int @@ partial = "%identity" |};;
run_exact_structure_primitive_mode `Logical
  {| external f_logical : int -> int @@ logical = "%identity" |};;
run_exact_structure_primitive_mode `Nonlogical
  {| external f_nonlogical : int -> int @@ nonlogical = "%identity" |};;

[%%expect{|
Has exact total/Total structure primitive mode
- : unit = ()
Has exact partial/Partial structure primitive mode
- : unit = ()
Has exact logical/Logical structure primitive mode
- : unit = ()
Has exact nonlogical/Physical structure primitive mode
- : unit = ()
|}];;

let contains (string : string) (substring : string) =
  let string_length = String.length string in
  let substring_length = String.length substring in
  let rec loop pos =
    pos + substring_length <= string_length
    &&
    (String.sub string pos substring_length = substring
     || loop (pos + 1))
  in
  substring_length = 0 || loop 0
;;

let check_printtyped_modality s =
  let parsed = Parse.implementation (Lexing.from_string s) in
  let typed, _, _, _, _, _ =
    Typemod.type_structure (Lazy.force Env.initial) parsed
  in
  let printed = Format.asprintf "%a" Printtyped.implementation typed in
  let old_aggregate = "join_const(unique,uncontended,physical" in
  let new_aggregate = "join_const(unique,uncontended,nonlogical" in
  if contains printed old_aggregate
     || contains printed "logicality: physical"
     || not (contains printed new_aggregate)
     || not (contains printed "logicality: nonlogical")
  then failwith "Printtyped did not use modality logicality spelling";
  Format.printf "Printtyped uses nonlogical for modality logicality@."
;;

[%%expect{|
val contains : string -> string -> bool = <fun>
val check_printtyped_modality : string -> unit = <fun>
|}];;

check_printtyped_modality
  {|
    [@@@warning "-220"]
    module type S = sig val x : int -> int @@ nonlogical end
  |};;

[%%expect{|
Printtyped uses nonlogical for modality logicality
- : unit = ()
|}];;

run {| let f : 'a. ('a @ portable -> 'a) = fun x -> x |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f : 'a. ('a -> 'a @ portable) = assert false |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f : 'a. ('a -> 'a) @ portable = fun x -> x |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = int @ portable -> int |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = int -> (int -> int) @ portable |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let x : int @ portable = 0 |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| module type S = sig @@ portable end |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| module type S = sig include sig end @@ portable end |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let foo = (10 : int @ portable) |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = { foo : int -> int @@ portable } |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = Foo of (int -> int) @@ portable |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = Foo of { x : int -> int @@ portable } |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| external id : 'a -> 'a @@ portable = "%identity" |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| module type S = sig val foo : int -> int @@ portable end |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f a b : _ @ portable = () |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f (a @ portable) b = () |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f a (b @ portable) = () |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let foo x y @ portable = ()|};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let foo = fun x y @ portable -> () |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;
