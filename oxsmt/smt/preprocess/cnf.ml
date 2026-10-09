(* Tseitin clausifier for the boolean skeleton (DESIGN.md §5; ADR-0003 Decision 2 for the
   atom/connective split). Two phases keep variable numbering purely tag-ordered
   (INVARIANTS.md I6), independent of hash-table traversal:

   Phase A collects every Bool-sorted subterm that needs its own variable — the atoms
   (leaves) and the [And]/[Or]/iff/[Ite] connective nodes — then sorts them by hash-cons
   tag and hands out ids 1..n. [Not] nodes get no variable: their literal is the negated
   literal of the child.

   Phase B walks those nodes in tag order and emits each connective's full biconditional
   Tseitin clauses, then a unit clause asserting the root literal. *)

open Oxsmt_core

module Lit = struct
  (* Signed non-zero int: +v is the positive literal on variable v, -v its negation. *)
  type t = int

  let make v ~positive =
    if v < 1 then invalid_arg "Cnf.Lit.make: variable must be >= 1";
    if positive then v else -v
  ;;

  let var l = abs l
  let is_positive l = l > 0
  let negate l = -l
  let to_dimacs l = l
  let equal (a : t) b = a = b
  let compare (a : t) b = Int.compare a b
end

module Clause = struct
  type t = Lit.t list
end

type t =
  { num_vars : int
  ; clauses : Clause.t list
  ; subterm : Term.t array (* index 0 unused; [subterm.(v)] is what v denotes *)
  ; is_atom : bool array (* [is_atom.(v)]: v is an opaque theory atom *)
  ; var_of_atom : int Term.Table.t
  }

(* Left-to-right map: stdlib [List.map] evaluates constructor args right-to-left, which
   would still be deterministic but reverses child order; keep source order for readable,
   tag-ascending emission. *)
let rec map_lr f = function
  | [] -> []
  | x :: xs ->
    let y = f x in
    y :: map_lr f xs
;;

(* The Bool children of a connective node (reached only when [is_atom] is false, so the
   node is one of these five forms). *)
let connective_children (t : Term.t) =
  match t.node with
  | Not a -> [ a ]
  | And xs | Or xs -> Iarr.to_list xs
  | Eq (a, b) -> [ a; b ]
  | Ite (c, a, b) -> [ c; a; b ]
  | Bool_const _ | Int_const _ | App _ | Arith _ | Le _ ->
    invalid_arg "Cnf: connective_children on a non-connective"
;;

let clausify (formula : Term.t) =
  if not (Sort.equal formula.sort Sort.bool)
  then invalid_arg "Cnf.clausify: formula must be Bool-sorted";
  (* ---- Phase A: collect var-needing nodes, number them by tag. ---- *)
  let seen : unit Term.Table.t = Term.Table.create 256 in
  let var_nodes = ref [] in
  let rec collect (t : Term.t) =
    if Term.Table.mem seen t
    then ()
    else (
      Term.Table.add seen t ();
      if Theory_view.is_atom t
      then var_nodes := t :: !var_nodes
      else (
        match t.node with
        | Not a -> collect a (* Not: no variable, descend *)
        | _ ->
          var_nodes := t :: !var_nodes;
          List.iter collect (connective_children t)))
  in
  collect formula;
  let ordered =
    List.sort (fun a b -> Int.compare a.Term.tag b.Term.tag) !var_nodes |> Array.of_list
  in
  let num_vars = Array.length ordered in
  let subterm = Array.make (num_vars + 1) formula in
  let is_atom = Array.make (num_vars + 1) false in
  let var_of_atom = Term.Table.create 256 in
  let var_of_node = Term.Table.create 256 in
  Array.iteri
    (fun i node ->
       let v = i + 1 in
       subterm.(v) <- node;
       Term.Table.replace var_of_node node v;
       if Theory_view.is_atom node
       then (
         is_atom.(v) <- true;
         Term.Table.replace var_of_atom node v))
    ordered;
  (* ---- Phase B: emit clauses. ---- *)
  let clauses = ref [] in
  let emit c = clauses := c :: !clauses in
  let pos v = Lit.make v ~positive:true in
  let neg v = Lit.make v ~positive:false in
  (* Literal of a Bool subterm: [Not a] folds to the negated literal of [a]; every other
     var-needing node uses its own (positive) variable. *)
  let rec lit_of (t : Term.t) =
    match t.node with
    | Not a -> Lit.negate (lit_of a)
    | _ ->
      (match Term.Table.find_opt var_of_node t with
       | Some v -> pos v
       | None ->
         (* Precondition violated: a non-Bool subterm sits in the boolean skeleton, i.e.
            the input was not preprocessed (an Int-sorted [Ite] below a connective). *)
         invalid_arg
           "Cnf.clausify: non-preprocessed formula (Int-sorted Ite in boolean skeleton)")
  in
  Array.iter
    (fun node ->
       let g = Term.Table.find var_of_node node in
       if Theory_view.is_atom node
       then (
         (* Opaque theory-atom leaf. Only [Bool_const] needs a clause (force its value); a
           Bool-argument [Eq] never reaches here — it is a connective (iff) below. *)
         match node.Term.node with
         | Bool_const true -> emit [ pos g ]
         | Bool_const false -> emit [ neg g ]
         | _ -> ())
       else (
         match node.Term.node with
         | And xs ->
           (* g <-> /\ li : (¬g ∨ li) each, and (g ∨ ¬l1 ∨ … ∨ ¬ln). *)
           let ls = map_lr lit_of (Iarr.to_list xs) in
           List.iter (fun li -> emit [ neg g; li ]) ls;
           emit (pos g :: map_lr Lit.negate ls)
         | Or xs ->
           (* g <-> \/ li : (¬li ∨ g) each, and (¬g ∨ l1 ∨ … ∨ ln). *)
           let ls = map_lr lit_of (Iarr.to_list xs) in
           List.iter (fun li -> emit [ Lit.negate li; pos g ]) ls;
           emit (neg g :: ls)
         | Eq (a, b) ->
           (* Bool-sorted (iff) Eq — ADR-0003 Decision 2. g <-> (la <-> lb). *)
           let la = lit_of a
           and lb = lit_of b in
           emit [ neg g; Lit.negate la; lb ];
           emit [ neg g; la; Lit.negate lb ];
           emit [ pos g; la; lb ];
           emit [ pos g; Lit.negate la; Lit.negate lb ]
         | Ite (c, a, b) ->
           (* g <-> ite(c,a,b), canonical 4-clause definition. *)
           let lc = lit_of c
           and la = lit_of a
           and lb = lit_of b in
           emit [ Lit.negate lc; neg g; la ];
           emit [ Lit.negate lc; Lit.negate la; pos g ];
           emit [ lc; neg g; lb ];
           emit [ lc; Lit.negate lb; pos g ]
         | Bool_const _ | Le _ | App _ | Int_const _ | Arith _ | Not _ ->
           (* not reachable: non-atoms are exactly And/Or/Eq(bool)/Ite; Not has no var *)
           invalid_arg "Cnf.clausify: unexpected non-connective in phase B"))
    ordered;
  emit [ lit_of formula ];
  { num_vars; clauses = List.rev !clauses; subterm; is_atom; var_of_atom }
;;

let num_vars t = t.num_vars
let clauses t = t.clauses
let iter_clauses f t = List.iter f t.clauses

let check_var t v =
  if v < 1 || v > t.num_vars then invalid_arg "Cnf: variable out of range"
;;

let subterm_of_var t v =
  check_var t v;
  t.subterm.(v)
;;

let is_atom_var t v =
  check_var t v;
  t.is_atom.(v)
;;

let atom_of_var t v =
  check_var t v;
  if t.is_atom.(v) then Some t.subterm.(v) else None
;;

let var_of_atom t a = Term.Table.find_opt t.var_of_atom a
