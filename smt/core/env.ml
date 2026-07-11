(* Reserved built-in symbol names (ADR-0003 Decision 5). SMT-LIB uses [div]/[mod] as
   theory operators, not as user function names, so re-declaring them is a client error:
   [declare_fun]/[declare_sort] raise [Reserved_symbol] rather than silently clobbering
   the pre-declared reserved ranks (R2). *)
let div_name = "div"
let mod_name = "mod"
let is_reserved name = String.equal name div_name || String.equal name mod_name

exception Reserved_symbol of string

type t =
  { ranks : (Symbol.t, Rank.t) Hashtbl.t
  ; div_sym : Symbol.t
  ; mod_sym : Symbol.t
  }

let create () =
  let ranks = Hashtbl.create 64 in
  let div_sym = Symbol.intern div_name in
  let mod_sym = Symbol.intern mod_name in
  let int_int_int = Rank.create [ Sort.int; Sort.int ] Sort.int in
  Hashtbl.replace ranks div_sym int_int_int;
  Hashtbl.replace ranks mod_sym int_int_int;
  { ranks; div_sym; mod_sym }
;;

let declare_sort _t name =
  if is_reserved name then raise (Reserved_symbol name);
  Symbol.intern name
;;

let declare_fun t name rank =
  if is_reserved name then raise (Reserved_symbol name);
  let sym = Symbol.intern name in
  Hashtbl.replace t.ranks sym rank;
  sym
;;

let rank t sym = Hashtbl.find t.ranks sym
let div_sym t = t.div_sym
let mod_sym t = t.mod_sym
