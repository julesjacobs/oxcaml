(* Session layer wiring the frozen core, preprocessing/clausification, and the CDCL SAT
   core into a check-sat loop (DESIGN.md §3, §5). See session.mli for the contract, in
   particular THE SOUNDNESS RULE.

   Everything threads one Context/Env (ADR-0003 Decision 6): terms asserted across
   [assert_term]/[push]/[pop] share the tag stream and hash-consing, so the same atom maps
   to the same SAT variable throughout the session. *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat
module Preprocess = Oxsmt_preprocess.Preprocess
module Cnf = Oxsmt_preprocess.Cnf

type verdict =
  | Sat
  | Unsat
  | Unknown

type t =
  { env : Env.t
  ; ctx : Context.t
  ; pp : Preprocess.t
  ; sat : Sat.t
  ; atom_to_var : Sat.var Term.Table.t
    (* one SAT var per distinct theory-atom / propositional-variable term, shared across
         assertions via hash-cons identity *)
  ; mutable bool_consts : (string * Sat.var) list
    (* nullary Bool-App atoms (propositional variables), for [get_model] *)
  ; mutable frames :
      Sat.var list (* selector stack, innermost first; base always present *)
  ; mutable has_theory : bool
    (* any Le / non-Bool Eq / applied predicate has been asserted: the SAT core cannot
         reason about it, so a propositional Sat is theory-unsound (see .mli) *)
  ; mutable degraded : bool (* Overflow/Unsupported seen: verdict must be Unknown (I8) *)
  ; mutable last_real_sat : bool (* last check_sat was a genuine (non-degraded) Sat *)
  }

let create () =
  let env = Env.create () in
  let ctx = Context.create env in
  let sat = Sat.create () in
  let base = Sat.new_var sat in
  { env
  ; ctx
  ; pp = Preprocess.create env ctx
  ; sat
  ; atom_to_var = Term.Table.create 256
  ; bool_consts = []
  ; frames = [ base ]
  ; has_theory = false
  ; degraded = false
  ; last_real_sat = false
  }
;;

let env t = t.env
let context t = t.ctx

(* Declarations reject the reserved fresh-symbol namespace (board #48), so a user symbol
   can never collide with one preprocessing invents. *)
let guard_name name =
  if Preprocess.is_reserved_name name
  then
    invalid_arg
      (Printf.sprintf "Session: cannot declare reserved internal symbol %s" name)
;;

let declare_sort t name =
  guard_name name;
  Env.declare_sort t.env name
;;

let declare_fun t name rank =
  guard_name name;
  Env.declare_fun t.env name rank
;;

let declare_const t name sort = declare_fun t name (Rank.create [] sort)

(* A theory atom is anything the propositional core cannot itself reason about: an order
   atom, a non-Bool equality, or an applied (arity >= 1) predicate. A nullary Bool [App]
   is a plain propositional variable, and [Bool_const] is a constant — neither is a theory
   atom. *)
let is_theory_atom (a : Term.t) =
  match a.node with
  | Le _ -> true
  | Eq _ -> true (* atom Eq always has non-Bool args (Bool-Eq is a connective) *)
  | App (_, args) -> Iarr.length args > 0
  | Bool_const _ -> false
  | Int_const _ | Arith _ | Not _ | And _ | Or _ | Ite _ -> false
;;

let current_selector t = List.hd t.frames

(* Map a clausified formula's local variable to a persistent SAT variable: atom variables
   share one SAT var per distinct atom term (hash-cons identity); auxiliary Tseitin
   variables are fresh per formula (kept in [local]). *)
let assert_clausified t cnf =
  let n = Cnf.num_vars cnf in
  let local = Array.make (n + 1) None in
  let sat_var v =
    if Cnf.is_atom_var cnf v
    then (
      let atom = Cnf.subterm_of_var cnf v in
      if is_theory_atom atom then t.has_theory <- true;
      match Term.Table.find_opt t.atom_to_var atom with
      | Some sv -> sv
      | None ->
        let sv = Sat.new_var t.sat in
        Term.Table.add t.atom_to_var atom sv;
        (match atom.node with
         | App (sym, args) when Iarr.length args = 0 && Sort.equal atom.sort Sort.bool ->
           t.bool_consts <- (Symbol.name sym, sv) :: t.bool_consts
         | _ -> ());
        sv)
    else (
      match local.(v) with
      | Some sv -> sv
      | None ->
        let sv = Sat.new_var t.sat in
        local.(v) <- Some sv;
        sv)
  in
  let lit_of (l : Cnf.Lit.t) =
    let sv = sat_var (Cnf.Lit.var l) in
    if Cnf.Lit.is_positive l then Sat.pos sv else Sat.neg sv
  in
  let sel = current_selector t in
  Cnf.iter_clauses
    (fun clause ->
       (* frame activation: clause holds only when the frame selector is assumed true *)
       Sat.add_clause t.sat (Sat.neg sel :: List.map lit_of clause))
    cnf
;;

let assert_term t term =
  match Preprocess.run t.pp term with
  | exception Term.Overflow -> t.degraded <- true
  | exception Term.Unsupported _ -> t.degraded <- true
  | pterm ->
    (match Cnf.clausify pterm with
     | exception _ -> t.degraded <- true
     | cnf -> assert_clausified t cnf)
;;

let push t = t.frames <- Sat.new_var t.sat :: t.frames

let pop t =
  match t.frames with
  | [ _ ] | [] -> invalid_arg "Session.pop: no matching push"
  | _ :: rest -> t.frames <- rest
;;

let check_sat t =
  t.last_real_sat <- false;
  if t.degraded
  then Unknown
  else (
    let assumptions = List.map Sat.pos t.frames in
    match Sat.solve ~assumptions t.sat with
    | Sat.Unsat -> Unsat (* propositional unsat is sound: theories only remove models *)
    | Sat.Sat ->
      if t.has_theory
      then Unknown (* SAT model may be theory-inconsistent (see .mli) *)
      else (
        t.last_real_sat <- true;
        Sat))
;;

let get_model t =
  if not t.last_real_sat
  then None
  else
    Some
      (List.sort
         (fun (a, _) (b, _) -> String.compare a b)
         (List.map (fun (name, sv) -> name, Sat.value t.sat sv) t.bool_consts))
;;

let stats t = Sat.stats t.sat
