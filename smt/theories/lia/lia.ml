(* LIA decision procedure. See lia.mli.

   Layered on {!Simplex}: this module's job is the atom <-> bound translation (reading the
   ADR-0003 [Le]-normal form, taking the exact ℤ complement of a negated atom), variable
   bookkeeping (problem vars for term leaves, deduplicated slacks for compound linear
   forms), and the branch-and-bound driver over the rational simplex. *)

open Oxsmt_core

exception Unsupported of string
exception Poisoned

(* Simplex premise tokens: user atoms carry the caller's ['tok]; B&B branch bounds carry
   an internal marker so they never masquerade as an input-core premise. *)
type 'tok reason =
  | User of 'tok
  | Branch of int

type 'tok conflict =
  { premises : 'tok list
  ; farkas : Rational.t list
  }

type 'tok result =
  | Sat_candidate
  | Conflict of 'tok conflict

type 'tok integer_result =
  | Int_sat of (Term.t * int) list
  | Int_unsat of 'tok conflict option
  | Int_unknown

(* A registered atom's positive-polarity reading: a bound [var <sense> rhs]. *)
type reg =
  { atom : Term.t
  ; var : int
  ; is_upper : bool
  ; rhs : Delta.t
  }

type 'tok t =
  { ctx : Context.t
  ; simplex : 'tok reason Simplex.t
  ; var_of_term : int Term.Table.t (* problem-var term -> simplex id *)
  ; problem_vars : (int * Term.t) Dynarray.t (* (simplex id, term), creation order *)
  ; slacks : ((int * int) list, int) Hashtbl.t (* sorted (varid,coeff) key -> slack id *)
  ; registered : reg Dynarray.t
  ; mutable overflows : int (* number of overflow-degradations to unknown *)
  }

let default_budget = 2000

let create ctx =
  { ctx
  ; simplex = Simplex.create ()
  ; var_of_term = Term.Table.create 64
  ; problem_vars = Dynarray.create ()
  ; slacks = Hashtbl.create 64
  ; registered = Dynarray.create ()
  ; overflows = 0
  }
;;

(* Refuse to reason on a bricked instance (an overflow left the tableau mid-pivot, so any
   verdict would be unsound — CONTRACT: discard, don't reuse). Every public entry that
   could read or extend solver state calls this first. *)
let ensure_live t = if Simplex.is_poisoned t.simplex then raise Poisoned

(* Get or create the problem variable for a term leaf. *)
let problem_var t (term : Term.t) =
  match Term.Table.find_opt t.var_of_term term with
  | Some id -> id
  | None ->
    let id = Simplex.new_problem_var t.simplex in
    Term.Table.replace t.var_of_term term id;
    Dynarray.add_last t.problem_vars (id, term);
    id
;;

(* Linear combination of an Int-sorted term: (problem-var id, coeff) pairs + integer
   const. [Arith] nodes carry the normalized form; a bare App/leaf is [1·leaf];
   [Int_const] is a pure constant. Int-[Ite] must have been removed by preprocessing. *)
let combo_of_term t (term : Term.t) : (int * int) list * int =
  match term.node with
  | Arith { coeffs; const } ->
    let pairs =
      Iarr.fold (fun acc (tm, c) -> (problem_var t tm, c) :: acc) [] coeffs |> List.rev
    in
    pairs, const
  | Int_const k -> [], k
  | Ite _ -> raise (Unsupported "LIA: Int-Ite must be removed by preprocessing")
  | _ -> [ problem_var t term, 1 ], 0
;;

let sort_key (pairs : (int * int) list) =
  List.sort (fun (a, _) (b, _) -> Int.compare a b) pairs
;;

(* The simplex variable carrying a linear combination, and whether the reported bound is a
   direct problem-var bound. Coeff-1 singletons bound their variable directly (DdM);
   anything else uses a deduplicated slack [s = Σ coeff·x]. *)
let var_for_combo t (pairs : (int * int) list) =
  match pairs with
  | [ (x, 1) ] -> x
  | _ ->
    let key = sort_key pairs in
    (match Hashtbl.find_opt t.slacks key with
     | Some s -> s
     | None ->
       let s =
         Simplex.new_slack t.simplex (List.map (fun (x, c) -> x, Rational.of_int c) pairs)
       in
       Hashtbl.replace t.slacks key s;
       s)
;;

(* Read [atom] (an [Le] or Int [Eq]) at [polarity] into simplex assertions [f]. [f] is
   [`Upper]/[`Lower] applied to (var, δ-rhs). Returns the list of (var, sense, rhs) so
   callers can either assert (setting bounds) or register (recording for propagation). *)
let constraints_of_atom t (atom : Term.t) ~polarity
  : (int * [ `Upper | `Lower ] * Delta.t) list
  =
  match atom.node with
  | Le inner ->
    let pairs, const = combo_of_term t inner in
    if pairs = [] then raise (Unsupported "LIA: constant Le atom (should be folded)");
    let var = var_for_combo t pairs in
    if polarity
    then
      (* Σ coeff·x + const <= 0 ==> var <= -const *)
      [ var, `Upper, Delta.of_rat (Rational.of_int (-const)) ]
    else
      (* ¬(inner <= 0) ≡ inner >= 1 (exact ℤ complement) ==> var >= 1 - const *)
      [ var, `Lower, Delta.of_rat (Rational.of_int (1 - const)) ]
  | Eq (a, b) when not (Sort.equal a.sort Sort.bool) ->
    if not polarity then raise (Unsupported "LIA: disequality needs a trichotomy split");
    (* a = b ==> combo(a) - combo(b) = 0 ==> Σ coeff·x = -(const_a - const_b) *)
    let pa, ca = combo_of_term t a in
    let pb, cb = combo_of_term t b in
    let merged =
      let tbl = Hashtbl.create 16 in
      List.iter
        (fun (x, c) ->
           Hashtbl.replace
             tbl
             x
             (c
              +
              try Hashtbl.find tbl x with
              | Not_found -> 0))
        pa;
      List.iter
        (fun (x, c) ->
           Hashtbl.replace
             tbl
             x
             ((try Hashtbl.find tbl x with
               | Not_found -> 0)
              - c))
        pb;
      Hashtbl.fold (fun x c acc -> if c = 0 then acc else (x, c) :: acc) tbl []
    in
    if merged = [] then raise (Unsupported "LIA: trivial equality (should be folded)");
    let var = var_for_combo t merged in
    let rhs = Delta.of_rat (Rational.of_int (-(ca - cb))) in
    [ var, `Upper, rhs; var, `Lower, rhs ]
  | _ -> raise (Unsupported "LIA: atom is neither Le nor an Int equality")
;;

let assert_atom t atom ~polarity ~premise =
  ensure_live t;
  List.iter
    (fun (var, sense, rhs) ->
       let _ : _ Simplex.conflict option =
         match sense with
         | `Upper -> Simplex.assert_upper t.simplex var rhs (User premise)
         | `Lower -> Simplex.assert_lower t.simplex var rhs (User premise)
       in
       ())
    (constraints_of_atom t atom ~polarity)
;;

let register_atom t (atom : Term.t) =
  ensure_live t;
  (* Record the positive reading of a [Le] atom for bound-propagation. Equality atoms are
     not propagation targets in v1. *)
  match atom.node with
  | Le _ ->
    if not (Dynarray.exists (fun r -> Term.equal r.atom atom) t.registered)
    then (
      match constraints_of_atom t atom ~polarity:true with
      | [ (var, `Upper, rhs) ] ->
        Dynarray.add_last t.registered { atom; var; is_upper = true; rhs }
      | [ (var, `Lower, rhs) ] ->
        Dynarray.add_last t.registered { atom; var; is_upper = false; rhs }
      | _ -> ())
  | _ -> ()
;;

(* Map an internal simplex conflict to the public one, dropping any [Branch] premise
   (which only appears inside {!solve_integer}'s branches). *)
let externalize (c : _ Simplex.conflict) : 'tok conflict =
  let premises, farkas =
    List.fold_right2
      (fun p f (ps, fs) ->
         match p with
         | User tok -> tok :: ps, f :: fs
         | Branch _ -> ps, fs)
      c.premises
      c.farkas
      ([], [])
  in
  { premises; farkas }
;;

let check t =
  ensure_live t;
  match Simplex.check t.simplex with
  | None -> Sat_candidate
  | Some c -> Conflict (externalize c)
;;

let rational_value t (term : Term.t) =
  ensure_live t;
  match Term.Table.find_opt t.var_of_term term with
  | Some id -> Delta.c_part (Simplex.value t.simplex id)
  | None -> Rational.zero
;;

let value_is_integer d = Delta.is_rational d && Rational.is_int (Delta.c_part d)

(* Lowest-tag problem variable whose current value is non-integer (needs branching).
   Branching (below) floors the c_part only. That is exact here because the LIA atom
   translation never emits a δ bound (every assert_atom bound has k=0), so problem-var
   values are always δ=0; a hypothetical strict δ bound wired through this layer would
   still branch soundly (x<=floor ∨ x>=floor+1 partitions ℤ) but could spin to the budget. *)
let first_non_integer t =
  let best = ref None in
  Dynarray.iter
    (fun (id, term) ->
       let d = Simplex.value t.simplex id in
       if not (value_is_integer d)
       then (
         match !best with
         | Some (bt, _, _) when bt.Term.tag <= term.Term.tag -> ()
         | _ -> best := Some (term, id, d)))
    t.problem_vars;
  !best
;;

let suggest_branch t =
  ensure_live t;
  match first_non_integer t with
  | None -> None
  | Some (term, _, d) ->
    let f = Rational.floor (Delta.c_part d) in
    let le_atom = Context.le t.ctx term (Context.int_const t.ctx f) in
    let ge_atom = Context.ge t.ctx term (Context.int_const t.ctx (f + 1)) in
    Some (le_atom, ge_atom)
;;

let extract_model t =
  Dynarray.fold_left
    (fun acc (id, term) ->
       let d = Simplex.value t.simplex id in
       if not (value_is_integer d)
       then failwith "Lia.model: variable is not integral (call after Int_sat)";
       (term, Rational.num (Delta.c_part d)) :: acc)
    []
    t.problem_vars
  |> List.sort (fun (a, _) (b, _) -> Int.compare a.Term.tag b.Term.tag)
;;

let model t =
  ensure_live t;
  extract_model t
;;

let solve_integer ?(budget = default_budget) t =
  ensure_live t;
  let splits = ref 0 in
  (* [root_conflict] captures a ℚ-level conflict found with no branch in scope, so a
     genuine single-Farkas certificate can be surfaced. *)
  let root_conflict = ref None in
  let rec dfs ~depth0 =
    match Simplex.check t.simplex with
    | Some c ->
      if depth0 then root_conflict := Some (externalize c);
      `Unsat
    | None ->
      (match first_non_integer t with
       | None -> `Sat (extract_model t)
       | Some (_, id, d) ->
         if !splits >= budget
         then `Unknown
         else (
           incr splits;
           let f = Rational.floor (Delta.c_part d) in
           let lo = Delta.of_rat (Rational.of_int f) in
           let hi = Delta.of_rat (Rational.of_int (f + 1)) in
           Simplex.push t.simplex;
           let _ = Simplex.assert_upper t.simplex id lo (Branch id) in
           let r1 = dfs ~depth0:false in
           Simplex.pop t.simplex 1;
           match r1 with
           | (`Sat _ | `Unknown) as r -> r
           | `Unsat ->
             Simplex.push t.simplex;
             let _ = Simplex.assert_lower t.simplex id hi (Branch id) in
             let r2 = dfs ~depth0:false in
             Simplex.pop t.simplex 1;
             r2))
  in
  (* Native-int incompleteness ceiling (I8): DdM pivoting and Farkas combinations grow
     coefficients internally, so guarded rational arithmetic can overflow mid-solve on
     small, non-adversarial inputs. Raising is sound; here — the complete decision driver
     — we degrade that to [Int_unknown] and count it as a distinct stat so a benchmark
     pass-rate gap is attributable, not a mystery. The fix is arbitrary-precision
     rationals (tracked as the core-bignum row, post-M4). *)
  match dfs ~depth0:true with
  | `Sat m -> Int_sat m
  | `Unknown -> Int_unknown
  | `Unsat -> Int_unsat !root_conflict
  | exception Rational.Overflow ->
    t.overflows <- t.overflows + 1;
    Int_unknown
;;

let propagate t =
  ensure_live t;
  let out = ref [] in
  Dynarray.iter
    (fun r ->
       (* atom's positive reading is [var <sense> rhs]. TRUE if the current bound already
         entails it; FALSE if the current opposite bound refutes it. Explanation = the
         single entailing bound (Lia_bound). *)
       let up = Simplex.get_upper t.simplex r.var in
       let lo = Simplex.get_lower t.simplex r.var in
       let emit polarity prem = out := (r.atom, polarity, [ prem ]) :: !out in
       if r.is_upper
       then (
         (* atom: var <= rhs *)
         match up with
         | Some (User tok, u) when Delta.le u r.rhs -> emit true tok
         | _ ->
           (match lo with
            | Some (User tok, l) when Delta.lt r.rhs l -> emit false tok
            | _ -> ()))
       else (
         (* atom: var >= rhs *)
         match lo with
         | Some (User tok, l) when Delta.le r.rhs l -> emit true tok
         | _ ->
           (match up with
            | Some (User tok, u) when Delta.lt u r.rhs -> emit false tok
            | _ -> ())))
    t.registered;
  List.rev !out
;;

let push t =
  ensure_live t;
  Simplex.push t.simplex
;;

let pop t n =
  ensure_live t;
  Simplex.pop t.simplex n
;;

(* Diagnostics stay readable after poisoning (you need [overflow_count] precisely to
   attribute the brick). *)
let pivot_count t = Simplex.pivot_count t.simplex
let overflow_count t = t.overflows
let is_poisoned t = Simplex.is_poisoned t.simplex
