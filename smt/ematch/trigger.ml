(* Auto-trigger inference (ADR-0012 L3, tranche 3). When a lemma carries no explicit
   [:pattern], infer a trigger from the body by the standard recipe: the SMALLEST
   uninterpreted-function-headed subterms that together COVER every bound variable. See
   trigger.mli.

   This is purely a COMPLETENESS heuristic (§3): a trigger only decides WHICH ground
   instances the matcher generates, and every instance is a valid consequence of the
   lemma, so an over- or under-broad inferred trigger changes only how many refutations we
   find — never a verdict. If some qvar cannot be reached through any UF application (it
   occurs only inside arithmetic), no trigger covers it and inference returns [[]]: the
   lemma does not fire, and a live lemma degrades to a sound [unknown] (never a dropped
   forall). *)

open Oxsmt_core

(* Qvar head symbol -> its index in [qvars], by symbol identity (qvar count is tiny). *)
let qvar_index qvars sym =
  let n = Array.length qvars in
  let rec go k =
    if k >= n
    then None
    else (
      match (Qvar.to_term qvars.(k)).Term.node with
      | App (s, args) when Iarr.length args = 0 && Symbol.equal s sym -> Some k
      | _ -> go (k + 1))
  in
  go 0
;;

(* The set of qvar indices occurring in [t], as a sorted int list. A qvar is a nullary
   [App] whose head is one of [qvars]; anything else is walked structurally. *)
let qvars_in qvars t =
  let seen = Hashtbl.create 8 in
  let rec go (t : Term.t) =
    match t.node with
    | App (sym, args) when Iarr.length args = 0 ->
      (match qvar_index qvars sym with
       | Some i -> Hashtbl.replace seen i ()
       | None -> ())
    | App (_, args) -> Iarr.iter go args
    | Arith l -> Iarr.iter (fun (tm, _c) -> go tm) l.coeffs
    | Le a -> go a
    | Eq (a, b) ->
      go a;
      go b
    | Not a -> go a
    | And xs | Or xs -> Iarr.iter go xs
    | Ite (c, a, b) ->
      go c;
      go a;
      go b
    | Bool_const _ | Int_const _ -> ()
  in
  go t;
  List.sort Int.compare (Hashtbl.fold (fun i () acc -> i :: acc) seen [])
;;

(* Node count — the "size" used to prefer the smallest covering subterms (a deeper, more
   specific trigger fires on fewer terms, curbing runaway matching). *)
let rec size (t : Term.t) =
  match t.node with
  | App (_, args) -> Iarr.fold (fun n a -> n + size a) 1 args
  | Arith l -> Iarr.fold (fun n (tm, _c) -> n + size tm) 1 l.coeffs
  | Le a | Not a -> 1 + size a
  | Eq (a, b) -> 1 + size a + size b
  | And xs | Or xs -> Iarr.fold (fun n a -> n + size a) 1 xs
  | Ite (c, a, b) -> 1 + size c + size a + size b
  | Bool_const _ | Int_const _ -> 1
;;

(* Every uninterpreted-application subterm ([App] with arity >= 1) of [body] that contains
   at least one qvar, deduplicated by hash-cons tag. Arithmetic/order/equality/boolean
   nodes are NOT trigger roots (matcher fragment, L3) — but we still recurse THROUGH them
   to reach UF applications nested inside (e.g. [f(x)] inside [f(x) + 1 > 0]). *)
let candidates qvars body =
  let out = ref [] in
  let seen = Hashtbl.create 32 in
  let rec go (t : Term.t) =
    (match t.node with
     | App (_, args) when Iarr.length args > 0 ->
       if (not (Hashtbl.mem seen t.tag)) && qvars_in qvars t <> []
       then (
         Hashtbl.replace seen t.tag ();
         out := t :: !out)
     | _ -> ());
    match t.node with
    | App (_, args) -> Iarr.iter go args
    | Arith l -> Iarr.iter (fun (tm, _c) -> go tm) l.coeffs
    | Le a | Not a -> go a
    | Eq (a, b) ->
      go a;
      go b
    | And xs | Or xs -> Iarr.iter go xs
    | Ite (c, a, b) ->
      go c;
      go a;
      go b
    | Bool_const _ | Int_const _ -> ()
  in
  go body;
  !out
;;

let infer ?(inert_head = fun (_ : Symbol.t) -> false) ~qvars body =
  let n = Array.length qvars in
  if n = 0
  then [] (* zero-qvar lemma is a ground fact; the matcher fires it without a trigger *)
  else (
    (* Is a candidate's head symbol INERT — provably unable to ever seed matching?
       (candidates are always [App] with arguments, so a head always exists.) The caller
       marks such heads; the only current case is a Skolem function minted for a nested
       existential (chunk 2b): it occurs in exactly one lemma body and the only thing that
       could create a ground [skf(t)] term is that very lemma firing, which needs a
       [skf(t)] to match — a deadlock, so a trigger on it is inert. A ground-occurrence
       COUNT heuristic was tried and measured a net corpus regression: it also demoted
       user heads with no INITIAL ground occurrence that are legitimately created DURING
       search (by other lemmas), losing more files than the Skolem demotion gained.
       Restricting demotion to the provably-deadlocked (Skolem) heads keeps the gain
       without that loss. Never a soundness input — a mis-chosen trigger only changes
       which valid instances fire. *)
    let head_inert (c : Term.t) =
      match c.Term.node with
      | App (sym, _) -> inert_head sym
      | _ -> false
    in
    (* Non-inert-first, then smallest-first, then tag-tiebroken for determinism (I6). With
       the default [inert_head = fun _ -> false] no head is inert, so the first key is a
       constant tie and the order is byte-identical to the size/tag recipe. Among
       non-inert candidates the size/tag order is untouched, so only a provably-inert head
       is demoted. *)
    let cands =
      List.map (fun c -> c, qvars_in qvars c) (candidates qvars body)
      |> List.sort (fun (a, _) (b, _) ->
        let c = Bool.compare (head_inert a) (head_inert b) in
        if c <> 0
        then c
        else (
          let c = Int.compare (size a) (size b) in
          if c <> 0 then c else Int.compare a.Term.tag b.Term.tag))
    in
    (* Greedy minimal cover: add a candidate only when it covers a still-uncovered qvar. *)
    let covered = Array.make n false in
    let n_covered = ref 0 in
    let chosen = ref [] in
    List.iter
      (fun (c, idxs) ->
        if !n_covered < n
        then (
          let helps = List.exists (fun i -> not covered.(i)) idxs in
          if helps
          then (
            chosen := c :: !chosen;
            List.iter
              (fun i ->
                if not covered.(i)
                then (
                  covered.(i) <- true;
                  incr n_covered))
              idxs)))
      cands;
    (* Only a trigger that covers EVERY qvar is usable; a partial cover would leave a qvar
       unbound, so the matcher emits nothing anyway (matcher.mli). Return one conjunctive
       multi-trigger, or [] when some qvar is unreachable through UF applications. *)
    if !n_covered = n then [ List.rev !chosen ] else [])
;;
