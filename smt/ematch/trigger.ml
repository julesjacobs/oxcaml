(* Auto-trigger inference (ADR-0012 L3, tranche 3). When a lemma carries no explicit
   [:pattern], infer a trigger from the body by the standard recipe: select
   uninterpreted-function-headed applications that together cover every bound variable,
   preferring candidates that reduce the number of conjunctive joins before using term
   size as the selectivity tie-break. See trigger.mli.

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

let infer ~qvars body =
  let n = Array.length qvars in
  if n = 0
  then [] (* zero-qvar lemma is a ground fact; the matcher fires it without a trigger *)
  else (
    (* Precompute deterministic static tie-break costs. Coverage is chosen dynamically
       below; size and tag only decide between candidates with equal marginal coverage. *)
    let cands =
      List.map (fun c -> c, qvars_in qvars c, size c) (candidates qvars body)
      |> List.sort (fun (a, _, asize) (b, _, bsize) ->
        let c = Int.compare asize bsize in
        if c <> 0 then c else Int.compare a.Term.tag b.Term.tag)
    in
    let covered = Array.make n false in
    let n_covered = ref 0 in
    let gain idxs =
      List.fold_left (fun count i -> if covered.(i) then count else count + 1) 0 idxs
    in
    (* Greedy set cover with the objective in the right order for E-matching: reduce
       conjunctive joins first by covering as many unbound variables as possible; among
       equal-coverage candidates prefer the smaller, more selective term. *)
    let rec choose chosen =
      if !n_covered = n
      then Some (List.rev chosen)
      else (
        let best =
          List.fold_left
            (fun best ((candidate_term, idxs, candidate_size) as candidate) ->
               let candidate_gain = gain idxs in
               if candidate_gain = 0
               then best
               else (
                 match best with
                 | None -> Some (candidate, candidate_gain)
                 | Some ((best_term, _, best_size), best_gain) ->
                   if candidate_gain > best_gain
                      || (candidate_gain = best_gain
                          && (candidate_size < best_size
                              || (candidate_size = best_size
                                  && candidate_term.Term.tag < best_term.Term.tag)))
                   then Some (candidate, candidate_gain)
                   else best))
            None
            cands
        in
        match best with
        | None -> None
        | Some ((term, idxs, _), _) ->
          List.iter
            (fun i ->
               if not covered.(i)
               then (
                 covered.(i) <- true;
                 incr n_covered))
            idxs;
          choose (term :: chosen))
    in
    (* Only a trigger that covers EVERY qvar is usable; a partial cover would leave a qvar
       unbound, so the matcher emits nothing anyway (matcher.mli). Return one conjunctive
       multi-trigger, or [] when some qvar is unreachable through UF applications. *)
    match choose [] with
    | Some chosen -> [ chosen ]
    | None -> [])
;;
