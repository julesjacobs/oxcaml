(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

let run s =
  let pe = Parse.expression (Lexing.from_string s) in
  let te = Typecore.type_expression (Lazy.force Env.initial) pe in
  let ute = Untypeast.untype_expression te in
  Format.printf "%a@." Pprintast.expression ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

let roundtrip_structure s =
  Language_extension.enable Language_extension.Mode Language_extension.Alpha;
  Language_extension.enable
    Language_extension.Layouts Language_extension.Alpha;
  let structure = Parse.implementation (Lexing.from_string s) in
  let typed_structure, _, _, _, _, _ =
    Typemod.type_structure (Lazy.force Env.initial) structure
  in
  let structure = Untypeast.untype_structure typed_structure in
  let printed = Format.asprintf "%a" Pprintast.structure structure in
  let reparsed = Parse.implementation (Lexing.from_string printed) in
  ignore (Typemod.type_structure (Lazy.force Env.initial) reparsed);
  printed
;;

[%%expect{|
val roundtrip_structure : string -> string = <fun>
|}];;

let run_structure s = Format.printf "%s@." (roundtrip_structure s)
;;

[%%expect{|
val run_structure : string -> unit = <fun>
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
match None with | Some (Some _) -> () | _ -> ()
- : unit = ()
|}];;

run {| let open struct type t = { mutable x : int [@atomic] } end in
       let _ = fun (v : t) -> v.x in () |};;

[%%expect{|
let open struct type t = {
                  mutable x: int [@atomic ]} end in
  let _ = fun (v : t) -> v.x in ()
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
fun x y z -> function | w -> x y z w
- : unit = ()
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
fun x y z -> (function | w -> x y z w)
- : unit = ()
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
match None with | Some (Some _) -> () | _ -> ()
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
fun x y z -> function | w -> x y z w
- : unit = ()
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
fun x y z -> (function | w -> x y z w)
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle value binding type annotations. *)

run {| let foo : 'a. 'a -> 'a = fun x -> x in foo |}

[%%expect{|
let foo : ('a : value) . 'a -> 'a = fun x -> x in foo
- : unit = ()
|}];;

run {| let foo : type a . a -> a = fun x -> x in foo |}

[%%expect{|
let foo : ('a : value) . 'a -> 'a = fun (type a) -> (fun x -> x : a -> a) in
foo
- : unit = ()
|}];;

run {| let foo : ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
let (foo : 'a -> 'a) = ((fun x -> x : 'a -> 'a) : @ portable) in foo
- : unit = ()
|}];;

run {| let foo : 'a . ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
let foo : ('a : value) . ('a -> 'a) @ portable = (fun x -> x : @ portable) in
foo
- : unit = ()
|}];;

run {|
  let module M = struct type t = { x : int } end in
  fun x -> let M.{ x } = M.{ x } in x
|}

[%%expect{|
let module M = struct type t = {
                        x: int } end in
  fun x -> let M.{ x }  = let open M in { x } in x
- : unit = ()
|}];;

run {| let foo : 'a -> 'a = fun x -> x in foo |}

[%%expect{|
let (foo : 'a -> 'a) = (fun x -> x : 'a -> 'a) in foo
- : unit = ()
|}];;

let run s =
  let pe = Parse.implementation (Lexing.from_string s) in
  let te,_,_,_,_,_ = Typemod.type_structure (Lazy.force Env.initial) pe in
  let ute = Untypeast.untype_structure te in
  Format.printf "%a@." Pprintast.structure ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

(* That test would hang before ocaml/ocaml#14105 *)
run {|type t = (::);; let f (x : t) = match x with (::) -> 4|}

[%%expect{|
type t =
  | (::)
let f (x : t) = match x with | (::) -> 4
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle declaration modalities. *)

run_structure {|
  module State = struct
    type t = int
    external next : t -> t @@ portable = "%identity"
  end |};;

[%%expect{|
module State =
  struct type t = int
         external next : t -> t @@ portable = "%identity" end
- : unit = ()
|}];;

run_structure {|
  module type S = sig
    val x : int -> int @@ portable
  end |};;

[%%expect{|
module type S  = sig val x : int -> int @@ portable end
- : unit = ()
|}];;

run_structure {|
  [@@@warning "-220"]
  module type S = sig
    val total : int -> int @@ total
    val partial : int -> int @@ partial
    val logical : int -> int @@ logical
    val nonlogical : int -> int @@ nonlogical
  end |};;

[%%expect{|
[@@@warning "-220"]
module type S  =
  sig
    val total : int -> int @@ total
    val partial : int -> int @@ partial
    val logical : int -> int @@ logical
    val nonlogical : int -> int @@ nonlogical
  end
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

[%%expect{|
val contains : string -> string -> bool = <fun>
|}];;

let module_declaration_source placement token =
  let source =
    match placement with
    | "declaration-suffix" ->
      Printf.sprintf
        "module type S = sig module M : sig val x : int -> int end @@ %s end"
        token
    | "declaration-name" ->
      Printf.sprintf
        "module type S = sig module (M @@ %s) : sig val x : int -> int end end"
        token
    | "alias-suffix" ->
      Printf.sprintf
        "module type S = sig module A : sig end module M = A @@ %s end"
        token
    | "alias-name" ->
      Printf.sprintf
        "module type S = sig module A : sig end module (M @@ %s) = A end"
        token
    | "recursive-first" ->
      Printf.sprintf
        "module type S = sig module rec M : sig end @@ %s and N : sig end end"
        token
    | "recursive-and" ->
      Printf.sprintf
        "module type S = sig module rec M : sig end and N : sig end @@ %s end"
        token
    | _ -> assert false
  in
  "[@@@warning \"-220\"] " ^ source
;;

[%%expect{|
val module_declaration_source : string -> string -> string = <fun>
|}];;

let check_module_declaration_roundtrip placement token =
  let source = module_declaration_source placement token in
  let printed = roundtrip_structure source in
  if not (contains printed ("@@ " ^ token))
  then failwith "module declaration modality was dropped";
  Format.printf "Round-tripped %s with %s@." placement token
;;

[%%expect{|
val check_module_declaration_roundtrip : string -> string -> unit = <fun>
|}];;

let placements =
  [ "declaration-suffix";
    "declaration-name";
    "alias-suffix";
    "alias-name";
    "recursive-first";
    "recursive-and" ]
;;

let modality_tokens = [ "total"; "partial"; "logical"; "nonlogical" ];;

List.iter
  (fun placement ->
    List.iter
      (check_module_declaration_roundtrip placement)
      modality_tokens)
  placements
;;

[%%expect{|
val placements : string list =
  ["declaration-suffix"; "declaration-name"; "alias-suffix"; "alias-name";
   "recursive-first"; "recursive-and"]
val modality_tokens : string list =
  ["total"; "partial"; "logical"; "nonlogical"]
Round-tripped declaration-suffix with total
Round-tripped declaration-suffix with partial
Round-tripped declaration-suffix with logical
Round-tripped declaration-suffix with nonlogical
Round-tripped declaration-name with total
Round-tripped declaration-name with partial
Round-tripped declaration-name with logical
Round-tripped declaration-name with nonlogical
Round-tripped alias-suffix with total
Round-tripped alias-suffix with partial
Round-tripped alias-suffix with logical
Round-tripped alias-suffix with nonlogical
Round-tripped alias-name with total
Round-tripped alias-name with partial
Round-tripped alias-name with logical
Round-tripped alias-name with nonlogical
Round-tripped recursive-first with total
Round-tripped recursive-first with partial
Round-tripped recursive-first with logical
Round-tripped recursive-first with nonlogical
Round-tripped recursive-and with total
Round-tripped recursive-and with partial
Round-tripped recursive-and with logical
Round-tripped recursive-and with nonlogical
- : unit = ()
|}]
