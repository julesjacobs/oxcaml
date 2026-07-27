(* Give every expression in the file the SAME location record.

   This is not a contrivance. It is what [Ast_helper] does by default: a generator that
   omits [~loc] gets [!default_loc], so a whole generated region shares one location
   object, and sharing a location record is a normal and correct thing for a program
   transformation to do.

   It is here because an admitted obligation must be identified by something this cannot
   forge. Anything derived from a location is forgeable by exactly this mapper, and if it
   were the identity then two obligations sharing a location would both be admitted on the
   strength of one [assume]. *)

open Ast_mapper
open Parsetree

let shared = ref None

let share location =
  match !shared with
  | Some shared -> shared
  | None ->
    shared := Some location;
    location
;;

let mapper _config =
  { default_mapper with
    expr =
      (fun self expression ->
        let expression = default_mapper.expr self expression in
        { expression with pexp_loc = share expression.pexp_loc })
  }
;;

let () = register "share_locations" mapper
