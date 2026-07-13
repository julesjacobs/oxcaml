(* Reserved built-in symbol names (ADR-0003 Decision 5). SMT-LIB uses [div]/[mod] as
   theory operators, not as user function names, so re-declaring them is a client error:
   [declare_fun]/[declare_sort] raise [Reserved_symbol] rather than silently clobbering
   the pre-declared reserved ranks (R2).

   Reserved fresh-symbol namespace (ADR-0012 R1): the [".oxsmt."] prefix is the single
   source of truth here (preprocess/session/parser reference it). Public [declare_fun]/
   [declare_sort] reject it (the forge door is closed); legitimate reserved symbols are
   minted via the per-env capability [declare_reserved]. *)
let div_name = "div"
let mod_name = "mod"
let is_builtin_reserved name = String.equal name div_name || String.equal name mod_name
let reserved_prefix = ".oxsmt."

let is_reserved_name name =
  let p = reserved_prefix in
  String.length name >= String.length p && String.sub name 0 (String.length p) = p
;;

(* Bytes that no SMT-LIB symbol form — simple or quoted — can contain: [|] (0x7C) closes
   a quoted symbol and [\] (0x5C) is illegal inside one (lexer §3.1, smt/lexical/lexer.ml),
   and neither is in the simple-symbol charset. A name carrying one of these can ONLY
   arrive through the programmatic [Env] door, never through parsed input. Rejecting it at
   the public declaration doors (defense-in-depth for board #58) closes every present and
   future internal-marker namespace — e.g. the arrays [.oxsmt.arr.select|<sortkey>] and
   bitvector marker forms — at the root, independent of the [.oxsmt.] prefix. Reserved
   minting via [declare_reserved] is UNAFFECTED: that door gates only on the [.oxsmt.]
   prefix, so a cap holder can still mint a sort-key-bearing internal name that contains
   [|]. *)
let has_nonsymbol_byte name =
  let n = String.length name in
  let rec go i =
    if i >= n
    then false
    else (
      match name.[i] with
      | '|' | '\\' -> true
      | _ -> go (i + 1))
  in
  go 0
;;

exception Reserved_symbol of string

(* The capability is the owning env's identity (ADR-0012 per-env strengthening): a cap
   authorizes reserved minting on its own env only. Ids come off a process-global counter;
   the id is never observable (not in terms, models, or cache keys), so I6 is unaffected. *)
type reserved_cap = int

type t =
  { ranks : (Symbol.t, Rank.t) Hashtbl.t
  ; div_sym : Symbol.t
  ; mod_sym : Symbol.t
  ; id : int
  }

let next_id = ref 0

let fresh_id () =
  let i = !next_id in
  next_id := i + 1;
  i
;;

let create_with_cap () =
  let ranks = Hashtbl.create 64 in
  let div_sym = Symbol.intern div_name in
  let mod_sym = Symbol.intern mod_name in
  let int_int_int = Rank.create [ Sort.int; Sort.int ] Sort.int in
  Hashtbl.replace ranks div_sym int_int_int;
  Hashtbl.replace ranks mod_sym int_int_int;
  let id = fresh_id () in
  { ranks; div_sym; mod_sym; id }, id
;;

let create () = fst (create_with_cap ())

let declare_reserved cap t name rank =
  if cap <> t.id
  then invalid_arg "Env.declare_reserved: capability does not match this env";
  if not (is_reserved_name name)
  then
    invalid_arg
      (Printf.sprintf "Env.declare_reserved: %s is not a reserved (.oxsmt.*) name" name);
  let sym = Symbol.intern name in
  Hashtbl.replace t.ranks sym rank;
  sym
;;

let declare_sort _t name =
  if is_builtin_reserved name || is_reserved_name name || has_nonsymbol_byte name
  then raise (Reserved_symbol name);
  Symbol.intern name
;;

let declare_fun t name rank =
  if is_builtin_reserved name || is_reserved_name name || has_nonsymbol_byte name
  then raise (Reserved_symbol name);
  let sym = Symbol.intern name in
  Hashtbl.replace t.ranks sym rank;
  sym
;;

let rank t sym = Hashtbl.find t.ranks sym
let div_sym t = t.div_sym
let mod_sym t = t.mod_sym
