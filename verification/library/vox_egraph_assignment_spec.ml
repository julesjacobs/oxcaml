(* The candidate assignments that saturation enumerates for one rule, as an
   explicit list of cases. [assignments count vars] lists every list of
   length [List.length vars] whose entries run over the ids [count - 1] down
   to 0 and then -1 (no node); sorts are not filtered here. For n variables
   there are (count + 1)^n cases. [closed_cases] is closure at every root
   for each case; [Vox_egraph_assignment_proof] shows that over this
   enumeration it equals the trusted closure
   [Vox_egraph_saturation_spec.closed_rule]. *)

module C = Vox_egraph_closure_spec
module L = Vox_egraph_language_spec
module Q = Vox_egraph_match_spec
module R = Vox_egraph_rule_spec

type cases = End | Case of int list * cases [@@inductive]

let[@def] rec (append @ total) (first : cases @ immutable) (second : cases @ immutable) =
  match first with End -> second | Case (ids, rest) -> Case (ids, append rest second)

let[@def] rec (prepend @ total) (id : int) (suffixes : cases @ immutable) =
  match suffixes with
  | End -> End
  | Case (ids, rest) -> Case (id :: ids, prepend id rest)

let[@def] rec (choices @ total) (count : int) (suffixes : cases @ immutable) =
  if count <= 0 then prepend (-1) suffixes
  else append (prepend (count - 1) suffixes) (choices (count - 1) suffixes)
  [@@decreases if count > 0 then count else 0]

let[@def] rec (assignments @ total) (count : int) (vars : L.sort list @ immutable) =
  match vars with
  | [] -> Case ([], End)
  | _ :: vars -> choices count (assignments count vars)

let[@def] rec (closed_cases @ total) (graph : Q.graph @ immutable)
    (rule : R.rule @ immutable) (cases : cases @ immutable) =
  match cases with
  | End -> true
  | Case (ids, rest) -> C.closed_roots graph rule ids graph.count && closed_cases graph rule rest

