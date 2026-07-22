(* TEST
 flags = "-I ${ocamlsrcdir}/typing";
 include ocamlcommon;
 expect;
*)

open Types

let arrow binder result =
  Btype.newgenty
    (Tarrow
       ( (Nolabel, Mode.Alloc.legacy, Mode.Alloc.legacy, Some binder),
         Predef.type_int,
         result,
         commu_ok ))

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop offset =
    offset + needle_length <= haystack_length
    &&
    (String.sub haystack offset needle_length = needle
     || loop (offset + 1))
  in
  loop 0

let debug_print_observations =
  let outer = Ident.create_local "x" in
  let inner = Ident.create_local "x" in
  let type_ = arrow outer (arrow inner Predef.type_int) in
  let outer_name = Ident.unique_name outer in
  let inner_name = Ident.unique_name inner in
  assert (not (String.equal outer_name inner_name));
  let raw = Format.asprintf "%a" Rawprinttyp.type_expr type_ in
  let graph =
    Gprinttyp.make
      (Gprinttyp.params ~short_ids:true ~colorize:false ())
      [ Gprinttyp.Decoration.make [], Gprinttyp.node type_ ]
    |> Format.asprintf "%a" Gprinttyp.pp
  in
  assert (contains ~needle:outer_name raw);
  assert (contains ~needle:inner_name raw);
  assert (contains ~needle:outer_name graph);
  assert (contains ~needle:inner_name graph);
  [ "raw printer retains both distinct arrow-binder identities";
    "graph printer retains both distinct arrow-binder identities";
  ]

[%%expect {|
val arrow : Ident.t -> Types.type_expr -> Types.type_expr = <fun>
val contains : needle:String.t -> String.t -> bool = <fun>
val debug_print_observations : string list =
  ["raw printer retains both distinct arrow-binder identities";
   "graph printer retains both distinct arrow-binder identities"]
|}]
