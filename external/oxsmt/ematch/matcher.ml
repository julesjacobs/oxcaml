(* E-matcher v1 (ADR-0012 L3, tranche 2). Backtracking match of trigger patterns against
   the read-only congruence closure ({!Egraph_view}), modulo EUF equalities,
   budget-debited inside enumeration (R4). Deterministic (candidates/members in id order).
   See matcher.mli. *)

open Oxsmt_core

exception Budget_exhausted

(* A partial substitution is a [(int * Term.t) list]: qvar index -> its ground image.
   Small (one per qvar); a linear assoc list is deterministic and adequate. *)

(* Debit one enumeration step; raise the instant the generation budget is spent, BEFORE
   materializing the next candidate/join (R4 — a per-round cap would let an N^2
   cross-product build first). *)
let spend budget =
  if !budget <= 0 then raise Budget_exhausted;
  decr budget
;;

(* Map a qvar placeholder's head symbol to its index in [lemma.qvars], else [None]. Qvar
   count is tiny; a linear scan by symbol identity is deterministic. *)
let qvar_index (lemma : Lemma.t) sym =
  let n = Array.length lemma.qvars in
  let rec go k =
    if k >= n
    then None
    else (
      match (Qvar.to_term lemma.qvars.(k)).Term.node with
      | App (s, args) when Iarr.length args = 0 && Symbol.equal s sym -> Some k
      | _ -> go (k + 1))
  in
  go 0
;;

(* Extend [sigma] by binding qvar index [i] to ground term [g]: consistency-check an
   existing binding modulo congruence, else add. Returns the (single-element or empty)
   list of resulting substitutions. *)
let bind view sigma i g =
  match List.assoc_opt i sigma with
  | Some g' -> if view.Egraph_view.equal_if_registered g' g then [ sigma ] else []
  | None -> [ (i, g) :: sigma ]
;;

(* Match pattern subterm [p] (may contain qvars) against ground term [g] modulo EUF
   congruence, extending [sigma]; returns every resulting substitution. *)
let rec match_term view lemma budget p (g : Term.t) sigma =
  spend budget;
  match (p : Term.t).node with
  | App (sym, args) when Iarr.length args = 0 && qvar_index lemma sym <> None ->
    (* p is a qvar placeholder: bind it (or check consistency) against g. *)
    let i = Option.get (qvar_index lemma sym) in
    bind view sigma i g
  | App (f, pargs) when Iarr.length pargs > 0 ->
    (* Structural match modulo congruence: some member of g's class is [App (f, cargs)]
       with pargs matching cargs pairwise. *)
    let plist = Iarr.to_list pargs in
    List.concat_map
      (fun (m : Term.t) ->
         spend budget;
         match m.node with
         | App (f', cargs) when Symbol.equal f f' && Iarr.length cargs = Iarr.length pargs
           -> match_args view lemma budget plist (Iarr.to_list cargs) sigma
         | _ -> [])
      (view.Egraph_view.class_members g)
  | _ ->
    (* Ground pattern leaf (a nullary non-qvar App, [Int_const], etc.) — compare modulo
       congruence. (A trigger's qvars are all reached via the App/qvar arms above; this
       arm is a ground constant a trigger pins.) *)
    if view.Egraph_view.equal_if_registered p g then [ sigma ] else []

and match_args view lemma budget ps cs sigma =
  match ps, cs with
  | [], [] -> [ sigma ]
  | p :: ps', c :: cs' ->
    List.concat_map
      (fun sigma' -> match_args view lemma budget ps' cs' sigma')
      (match_term view lemma budget p c sigma)
  | _, _ -> [] (* arity mismatch: no match *)
;;

(* Match one trigger PATTERN (a UF application) against the e-graph roots, extending
   [sigma]. Root candidates come from [app_terms_by_symbol] (not from a class) so every
   registered [App f] is tried; arguments then match modulo congruence via [match_term]. A
   non-UF-application pattern root is out of matcher v1's fragment -> no matches. *)
let match_pattern view lemma budget (pat : Term.t) sigma =
  match pat.node with
  | App (f, pargs) when Iarr.length pargs > 0 ->
    let plist = Iarr.to_list pargs in
    List.concat_map
      (fun (cand : Term.t) ->
         spend budget;
         match cand.node with
         | App (f', cargs) when Symbol.equal f f' && Iarr.length cargs = Iarr.length pargs
           -> match_args view lemma budget plist (Iarr.to_list cargs) sigma
         | _ -> [])
      (view.Egraph_view.app_terms_by_symbol f)
  | _ -> []
;;

(* A conjunctive multi-trigger: every pattern must match under ONE shared substitution. *)
let match_conjunctive view lemma budget patterns sigma =
  List.fold_left
    (fun sigmas pat ->
       List.concat_map (fun s -> match_pattern view lemma budget pat s) sigmas)
    [ sigma ]
    patterns
;;

let substitutions view (lemma : Lemma.t) ~budget =
  let n = Array.length lemma.qvars in
  if n = 0
  then
    (* A zero-qvar lemma is [forall (). body] = the ground fact [body]; there is nothing
       to match, so it instantiates ONCE with the empty substitution regardless of
       triggers. (Matching a trigger would also yield the empty substitution, but a
       zero-qvar lemma need not carry one — [body] is unconditionally a valid instance of
       itself.) This is the [[||]] the interface contracts; returning [] here would
       silently drop the fact. *)
    [ [||] ]
  else (
    let out = ref [] in
    List.iter
      (fun alternative ->
         let sigmas = match_conjunctive view lemma budget alternative [] in
         List.iter
           (fun sigma ->
              (* Emit only fully-bound substitutions (every qvar covered). Indices in
               [sigma] are distinct by construction ([bind] adds at most once per index),
               so length = n iff all qvars are bound. *)
              if List.length sigma = n
              then out := Array.init n (fun i -> List.assoc i sigma) :: !out)
           sigmas)
      lemma.triggers;
    List.rev !out)
;;

(* Streaming form of [substitutions].  The legacy entry point above deliberately stays
   unchanged: [OXSMT_LEMMA_STREAM] is an A/B gate, and its OFF arm must retain the exact
   eager-list behavior.  The continuation form exposes each complete substitution as
   soon as it is found.  In particular, substitutions already handed to [yield] remain
   available to the manager if a later enumeration step raises [Budget_exhausted]. *)
let iter_substitutions view (lemma : Lemma.t) ~budget ~yield =
  let rec iter_match_term p (g : Term.t) sigma k =
    spend budget;
    match (p : Term.t).node with
    | App (sym, args) when Iarr.length args = 0 && qvar_index lemma sym <> None ->
      let i = Option.get (qvar_index lemma sym) in
      (match List.assoc_opt i sigma with
       | Some g' -> if view.Egraph_view.equal_if_registered g' g then k sigma
       | None -> k ((i, g) :: sigma))
    | App (f, pargs) when Iarr.length pargs > 0 ->
      let plist = Iarr.to_list pargs in
      List.iter
        (fun (m : Term.t) ->
           spend budget;
           match m.node with
           | App (f', cargs)
             when Symbol.equal f f' && Iarr.length cargs = Iarr.length pargs ->
             iter_match_args plist (Iarr.to_list cargs) sigma k
           | _ -> ())
        (view.Egraph_view.class_members g)
    | _ -> if view.Egraph_view.equal_if_registered p g then k sigma

  and iter_match_args ps cs sigma k =
    match ps, cs with
    | [], [] -> k sigma
    | p :: ps', c :: cs' ->
      iter_match_term p c sigma (fun sigma' -> iter_match_args ps' cs' sigma' k)
    | _, _ -> ()
  in
  let iter_pattern (pat : Term.t) sigma k =
    match pat.node with
    | App (f, pargs) when Iarr.length pargs > 0 ->
      let plist = Iarr.to_list pargs in
      List.iter
        (fun (cand : Term.t) ->
           spend budget;
           match cand.node with
           | App (f', cargs)
             when Symbol.equal f f' && Iarr.length cargs = Iarr.length pargs ->
             iter_match_args plist (Iarr.to_list cargs) sigma k
           | _ -> ())
        (view.Egraph_view.app_terms_by_symbol f)
    | _ -> ()
  in
  let rec iter_conjunctive patterns sigma k =
    match patterns with
    | [] -> k sigma
    | pattern :: rest ->
      iter_pattern pattern sigma (fun sigma' -> iter_conjunctive rest sigma' k)
  in
  let n = Array.length lemma.qvars in
  if n = 0
  then yield [||]
  else
    List.iter
      (fun alternative ->
         iter_conjunctive alternative [] (fun sigma ->
           if List.length sigma = n
           then yield (Array.init n (fun i -> List.assoc i sigma))))
      lemma.triggers
;;
