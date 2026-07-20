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

(* Value is [(Symbol.t * entry)] rather than just [entry] so the registry can be validated
   against the environment ({!validate_ranks}) — that check needs the [Symbol.t] to look
   up the recorded rank, and the [int] key alone cannot be turned back into a [Symbol.t]. *)
type t = { by_sym : (Symbol.t * entry) Id_map.t }

let empty = { by_sym = Id_map.empty }
let is_empty t = Id_map.is_empty t.by_sym
let id (s : Symbol.t) = (s :> int)

(* The canonical FULL rank an operator of this role over (index, element) must have:
   [Select : (Array(i,e), i) -> e], [Store : (Array(i,e), i, e) -> Array(i,e)]. Used to
   validate a registered symbol's actual environment rank ({!validate_ranks}) — arity
   alone is not enough: an op minted at the right arity but WRONG SORTS also passes to the
   sort-agnostic congruence engine and yields a wrong verdict. *)
let canonical_rank role ~index ~element =
  let arr = Sort.array_ ~index ~element in
  match role with
  | Select -> Rank.create [ arr; index ] element
  | Store -> Rank.create [ arr; index; element ] arr
;;

(* Full-signature equality of two ranks — same codomain and same domain sorts in order.
   Compared inline off [Rank.t]'s public record ([rank.mli] exposes [domain]/[codomain])
   so this touches no frozen interface. *)
let rank_matches (a : Rank.t) (b : Rank.t) =
  Sort.equal a.Rank.codomain b.Rank.codomain
  && Iarr.length a.Rank.domain = Iarr.length b.Rank.domain
  &&
  let rec loop i =
    i = Iarr.length a.Rank.domain
    || (Sort.equal (Iarr.get a.Rank.domain i) (Iarr.get b.Rank.domain i) && loop (i + 1))
  in
  loop 0
;;

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
  | Sort.Real -> "Real"
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
  | Some (_, existing) ->
    (* Idempotent on the identical entry; a genuine role/sort conflict is a front-end
       construction bug (a symbol is minted once per (role, index, element)). *)
    if entry_equal existing entry
    then t
    else invalid_arg "Array_defs: symbol already registered in a conflicting role"
  | None -> { by_sym = Id_map.add (id sym) (sym, entry) t.by_sym }
;;

let role_of_sym t sym = Option.map snd (Id_map.find_opt (id sym) t.by_sym)

(* Cross-check every registered operator's ACTUAL rank (looked up via [rank_of], which the
   caller backs with the session {!Env}) against the canonical FULL SIGNATURE for its role
   and sorts. The name-only check in [add] cannot catch this: [op_symbol_name] encodes the
   (role, index, element) but not the rank, so a caller can mint a canonical
   [.oxsmt.arr.*] name at the WRONG rank (e.g. via {!Internal_minter.mint}, whose admit
   gate is name-shape only) and register it. Arity agreement alone is NOT sufficient: an
   op minted at the right arity but WRONG argument/result SORTS (e.g. a select whose index
   argument is [Bool]) also flows into the sort-agnostic congruence engine, so the ROW
   rules would relate sort-mismatched terms — a wrong verdict. Requiring a full-signature
   [rank_matches] against [canonical_rank] closes both. Raises [Invalid_argument] on a
   disagreeing or missing rank. Mirrors the bitvector theory's rank-agreement discipline;
   the consuming-side arity guards in the arrays theory are the (arity-only) second layer. *)
let validate_ranks t ~(rank_of : Symbol.t -> Rank.t option) =
  Id_map.iter
    (fun _ (sym, entry) ->
      let want = canonical_rank entry.role ~index:entry.index ~element:entry.element in
      match rank_of sym with
      | Some r when rank_matches r want -> ()
      | Some _ ->
        invalid_arg
          (Printf.sprintf
             "Array_defs.validate_ranks: %s registered as %s does not have the canonical \
              rank for its index/element sorts (full-signature disagreement)"
             (Symbol.name sym)
             (match entry.role with
              | Select -> "select"
              | Store -> "store"))
      | None ->
        invalid_arg
          (Printf.sprintf
             "Array_defs.validate_ranks: registered operator %s has no rank in the \
              environment"
             (Symbol.name sym)))
    t.by_sym
;;
