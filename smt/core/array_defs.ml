(* Symbol.t is [private int], so it keys an immutable [Int] map by its underlying id
   (O(log n), functional [add]). The registry is tiny (a few (index, element)
   instantiations per query), so map overhead is irrelevant. Mirrors {!Datatype_defs}. *)
module Id_map = Map.Make (Int)

type role =
  | Select
  | Store

type entry =
  { role : role
  ; index : Sort.t
  ; element : Sort.t
  }

type t = { by_sym : entry Id_map.t }

let empty = { by_sym = Id_map.empty }
let is_empty t = Id_map.is_empty t.by_sym
let id (s : Symbol.t) = (s :> int)

(* A deterministic string identity for a sort. Distinct sorts yield distinct keys, so the
   generated operator name is unique per instantiation. *)
let rec sort_key (s : Sort.t) : string =
  match s with
  | Sort.Bool -> "Bool"
  | Sort.Int _ -> "Int"
  | Sort.Uninterpreted sym -> "U:" ^ Symbol.name sym
  | Sort.Datatype sym -> "D:" ^ Symbol.name sym
  | Sort.Array (i, e) -> Printf.sprintf "(A %s %s)" (sort_key i) (sort_key e)
  | Sort.BitVec w -> Printf.sprintf "(BV %d)" w
;;

(* The reserved sub-namespace every array op symbol lives under (board #58). Shared by
   [op_symbol_name] (which mints under it) and [is_op_sym] (which recognizes it). *)
let op_prefix = ".oxsmt.arr."

let op_symbol_name role ~index ~element =
  let role_str =
    match role with
    | Select -> "select"
    | Store -> "store"
  in
  Printf.sprintf "%s%s|%s|%s" op_prefix role_str (sort_key index) (sort_key element)
;;

(* An array [select]/[store] op symbol by NAME: the [.oxsmt.arr.] prefix plus at least one
   [|] sort-key separator (which the ext-witness Skolem [.oxsmt.arr.ext.N] lacks). This is
   how the session's assert-side reserved-symbol gate lets a legitimate op symbol through
   while still rejecting every other reserved name. A name-shape test is sufficient
   BECAUSE the door is what enforces provenance: a [.oxsmt.arr.*] name can only acquire a
   rank — and so be applied via [Context.app] — through the cap-gated
   [Env.declare_reserved], which only the parser (via the internal-mint hook) and the
   arrays theory hold. A user [Symbol.intern] of such a name gets a rank-less symbol that
   [Context.app] refuses; the public declare doors reject the prefix and the [|] byte
   outright. So any op-named symbol that reaches a built term is one this codebase minted,
   never a user alias. *)
let is_op_name (n : string) =
  String.starts_with ~prefix:op_prefix n && String.contains n '|'
;;

let is_op_sym (s : Symbol.t) = is_op_name (Symbol.name s)

let role_equal a b =
  match a, b with
  | Select, Select | Store, Store -> true
  | (Select | Store), _ -> false
;;

let entry_equal a b =
  role_equal a.role b.role && Sort.equal a.index b.index && Sort.equal a.element b.element
;;

let add t sym role ~index ~element =
  (* board #58 (soundness): the registry is caller-installable through the PUBLIC
     [Session.set_arrays] + this [add], and the arrays theory classifies an [App] head as
     a select/store purely by registry membership ([role_of_sym]). So the registry MUST be
     self-certifying: an entry may only claim the (role, index, element) that its symbol
     NAME canonically encodes. Otherwise a caller could register an arbitrary symbol (or a
     [.oxsmt.arr.ext.*] witness) as an operator and have the theory apply read-over-write
     / extensionality to it -> wrong verdict. Reject a mismatch with an explicit raise
     (release builds are -noassert, so never [assert]). Legitimate callers (the parser's
     [array_op_sym], the theory's [select_sym]) mint the symbol from exactly this
     [op_symbol_name], so they always pass. *)
  let canonical = op_symbol_name role ~index ~element in
  if not (String.equal (Symbol.name sym) canonical)
  then
    invalid_arg
      (Printf.sprintf
         "Array_defs.add: symbol %s is not the canonical op name (%s) for its claimed \
          role/sorts"
         (Symbol.name sym)
         canonical);
  let entry = { role; index; element } in
  match Id_map.find_opt (id sym) t.by_sym with
  | Some existing ->
    (* Idempotent on the identical entry; a genuine role/sort conflict is a front-end
       construction bug (a symbol is minted once per (role, index, element)). *)
    if entry_equal existing entry
    then t
    else invalid_arg "Array_defs: symbol already registered in a conflicting role"
  | None -> { by_sym = Id_map.add (id sym) entry t.by_sym }
;;

let role_of_sym t sym = Id_map.find_opt (id sym) t.by_sym
