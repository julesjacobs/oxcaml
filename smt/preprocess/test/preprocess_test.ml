(* Tests for smt/preprocess (ADR-0003 §5 obligations). The main oracle is brute-force
   EQUIVALENCE BY EVALUATION: a small, test-only evaluator over [Term.t] (Bool/Int values;
   uninterpreted functions via a deterministic total table; Int variables over a window)
   evaluates the original formula and the preprocessed one on every enumerated assignment,
   with fresh symbols resolved through the passes' computable witnesses (the definition
   list). Equal truth on the shared vocabulary is the property.

   For the clausifier the oracle enumerates Boolean assignments over the original opaque
   atoms and checks [original  <=>  the Tseitin extension satisfies every clause].

   Also: [Term.Debug.check ~mode:Pipeline] on pipeline output (incl. adversarial nests),
   determinism (same input -> identical CNF), and the [Unsupported] session-boundary
   contract. Stdlib-only, deterministic (fixed-seed xorshift PRNG), no wall-clock. *)

open Oxsmt_core
module Preprocess = Oxsmt_preprocess.Preprocess
module Cnf = Oxsmt_preprocess.Cnf

let checks = ref 0
let failures = ref 0

let check name cond =
  incr checks;
  if not cond
  then (
    incr failures;
    Printf.printf "  FAIL %s\n" name)
;;

let check_raises name f =
  incr checks;
  match f () with
  | exception _ -> ()
  | _ ->
    incr failures;
    Printf.printf "  FAIL %s (expected an exception, got none)\n" name
;;

(* ------------------------------------------------------------------ *)
(* Deterministic PRNG: xorshift64*, fixed seed (as in core_test). *)

let seed0 = 0x1E3779B97F4A7C15
let rng = ref seed0

let rand_bits () =
  let x = !rng in
  let x = x lxor (x lsr 12) in
  let x = x lxor (x lsl 25) in
  let x = x lxor (x lsr 27) in
  rng := x;
  x * 0x2545F4914F6CDD1D land max_int
;;

let rand_int n = rand_bits () mod n
let set_seed s = rng := s

(* ------------------------------------------------------------------ *)
(* Symbol-keyed hashtable for the evaluator's interpretation. *)

module SymTbl = Hashtbl.Make (struct
    type t = Symbol.t

    let equal = Symbol.equal
    let hash = Symbol.hash
  end)

(* ------------------------------------------------------------------ *)
(* The test-only evaluator. An interpretation fixes: values of nullary Int/Bool variables,
   a deterministic total function for each uninterpreted symbol (keyed on name + argument
   values, so congruence holds), euclidean div/mod, and the fresh-symbol definitions. *)

type interp =
  { ints : int SymTbl.t
  ; bools : bool SymTbl.t
  ; defs : Term.t SymTbl.t
  ; div_sym : Symbol.t
  ; mod_sym : Symbol.t
  }

(* Euclidean division: b <> 0, result 0 <= r < |b|. *)
let euclid a b =
  let ab = abs b in
  let r = ((a mod ab) + ab) mod ab in
  let q = (a - r) / b in
  q, r
;;

let mix name vals = List.fold_left (fun h v -> (h * 31) + v) (Hashtbl.hash name) vals
let func_int sym vals = (((mix (Symbol.name sym) vals mod 17) + 17) mod 17) - 8
let func_bool sym vals = mix (Symbol.name sym) vals land 1 = 1

let rec eval_int interp (t : Term.t) : int =
  match t.node with
  | Int_const k -> k
  | Arith l ->
    Iarr.fold
      (fun acc (tm, c) -> acc + (c * eval_int interp tm))
      l.Term.const
      l.Term.coeffs
  | Ite (c, a, b) -> if eval_bool interp c then eval_int interp a else eval_int interp b
  | App (sym, args) ->
    if Symbol.equal sym interp.div_sym
    then
      fst (euclid (eval_int interp (Iarr.get args 0)) (eval_int interp (Iarr.get args 1)))
    else if Symbol.equal sym interp.mod_sym
    then
      snd (euclid (eval_int interp (Iarr.get args 0)) (eval_int interp (Iarr.get args 1)))
    else if Iarr.length args = 0
    then (
      match SymTbl.find_opt interp.defs sym with
      | Some def -> eval_int interp def
      | None -> SymTbl.find interp.ints sym)
    else func_int sym (List.map (eval_int interp) (Iarr.to_list args))
  | Bool_const _ | Le _ | Eq _ | Not _ | And _ | Or _ -> failwith "eval_int: non-Int node"

and eval_bool interp (t : Term.t) : bool =
  match t.node with
  | Bool_const b -> b
  | Le a -> eval_int interp a <= 0
  | Eq (a, b) ->
    if Sort.equal a.sort Sort.bool
    then Bool.equal (eval_bool interp a) (eval_bool interp b)
    else Int.equal (eval_int interp a) (eval_int interp b)
  | Not a -> not (eval_bool interp a)
  | And xs -> Iarr.for_all (eval_bool interp) xs
  | Or xs -> Iarr.exists (eval_bool interp) xs
  | Ite (c, a, b) -> if eval_bool interp c then eval_bool interp a else eval_bool interp b
  | App (sym, args) ->
    if Iarr.length args = 0
    then (
      match SymTbl.find_opt interp.defs sym with
      | Some def -> eval_bool interp def
      | None -> SymTbl.find interp.bools sym)
    else func_bool sym (List.map (eval_int interp) (Iarr.to_list args))
  | Int_const _ | Arith _ -> failwith "eval_bool: non-Bool node"
;;

(* ------------------------------------------------------------------ *)
(* Structural predicates used to assert per-pass invariants directly. *)

let rec has_int_ite (t : Term.t) =
  (match t.node with
   | Ite _ -> Sort.equal t.sort Sort.int
   | _ -> false)
  || List.exists has_int_ite (children t)

and has_divmod_app (t : Term.t) =
  (match t.node with
   | App (sym, _) ->
     (match Symbol.name sym with
      | "div" | "mod" -> true
      | _ -> false)
   | _ -> false)
  || List.exists has_divmod_app (children t)

and children (t : Term.t) =
  match t.node with
  | Bool_const _ | Int_const _ -> []
  | App (_, xs) | And xs | Or xs -> Iarr.to_list xs
  | Arith l -> List.map fst (Iarr.to_list l.Term.coeffs)
  | Le a | Not a -> [ a ]
  | Eq (a, b) -> [ a; b ]
  | Ite (a, b, c) -> [ a; b; c ]
;;

(* ------------------------------------------------------------------ *)
(* A session with a fixed vocabulary. [make_session] is called fresh per determinism run
   so tag streams start clean (INVARIANTS.md I6). *)

type session =
  { env : Env.t
  ; ctx : Context.t
  ; pp : Preprocess.t
  ; x : Symbol.t array (* nullary Int variables *)
  ; p : Symbol.t array (* nullary Bool variables *)
  ; f : Symbol.t (* Int -> Int *)
  ; g : Symbol.t (* Int, Int -> Int *)
  ; pr : Symbol.t (* Int -> Bool *)
  }

let n_int_vars = 2
let n_bool_vars = 2

let make_session () =
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let x =
    Array.init n_int_vars (fun i ->
      Env.declare_fun env (Printf.sprintf "x%d" i) (Rank.create [] Sort.int))
  in
  let p =
    Array.init n_bool_vars (fun i ->
      Env.declare_fun env (Printf.sprintf "p%d" i) (Rank.create [] Sort.bool))
  in
  let f = Env.declare_fun env "f" (Rank.create [ Sort.int ] Sort.int) in
  let g = Env.declare_fun env "g" (Rank.create [ Sort.int; Sort.int ] Sort.int) in
  let pr = Env.declare_fun env "pr" (Rank.create [ Sort.int ] Sort.bool) in
  { env; ctx; pp = Preprocess.create cap env ctx; x; p; f; g; pr }
;;

(* ------------------------------------------------------------------ *)
(* Random term generators. Bounded depth; small constants and coefficients so evaluation
   stays well within native int. Divisors are nonzero constants (the supported fragment).
   These formulas contain value-Ite, div and mod: the constructs the passes target. *)

let small_divisors = [| 1; 2; 3; -2; -4 |]

let rec gen_int s depth : Term.t =
  let ctx = s.ctx in
  if depth <= 0
  then (
    match rand_int 2 with
    | 0 -> Context.const ctx s.x.(rand_int n_int_vars)
    | _ -> Context.int_const ctx (rand_int 7 - 3))
  else (
    match rand_int 9 with
    | 0 -> Context.const ctx s.x.(rand_int n_int_vars)
    | 1 -> Context.int_const ctx (rand_int 7 - 3)
    | 2 -> Context.add ctx (gen_int s (depth - 1)) (gen_int s (depth - 1))
    | 3 -> Context.sub ctx (gen_int s (depth - 1)) (gen_int s (depth - 1))
    | 4 -> Context.mul_const ctx (rand_int 5 - 2) (gen_int s (depth - 1))
    | 5 ->
      Context.ite
        ctx
        (gen_bool s (depth - 1))
        (gen_int s (depth - 1))
        (gen_int s (depth - 1))
    | 6 ->
      Context.div
        ctx
        (gen_int s (depth - 1))
        (Context.int_const ctx small_divisors.(rand_int 5))
    | 7 ->
      Context.mod_
        ctx
        (gen_int s (depth - 1))
        (Context.int_const ctx small_divisors.(rand_int 5))
    | _ ->
      (match rand_int 2 with
       | 0 -> Context.app ctx s.f [ gen_int s (depth - 1) ]
       | _ -> Context.app ctx s.g [ gen_int s (depth - 1); gen_int s (depth - 1) ]))

and gen_bool s depth : Term.t =
  let ctx = s.ctx in
  if depth <= 0
  then (
    match rand_int 4 with
    | 0 -> Context.const ctx s.p.(rand_int n_bool_vars)
    | 1 -> Context.le ctx (gen_int s 0) (gen_int s 0)
    | 2 -> Context.eq ctx (gen_int s 0) (gen_int s 0)
    | _ -> Context.app ctx s.pr [ gen_int s 0 ])
  else (
    match rand_int 9 with
    | 0 -> Context.const ctx s.p.(rand_int n_bool_vars)
    | 1 -> Context.le ctx (gen_int s (depth - 1)) (gen_int s (depth - 1))
    | 2 -> Context.lt ctx (gen_int s (depth - 1)) (gen_int s (depth - 1))
    | 3 -> Context.eq ctx (gen_int s (depth - 1)) (gen_int s (depth - 1))
    | 4 -> Context.app ctx s.pr [ gen_int s (depth - 1) ]
    | 5 -> Context.not_ ctx (gen_bool s (depth - 1))
    | 6 -> Context.and_ ctx [ gen_bool s (depth - 1); gen_bool s (depth - 1) ]
    | 7 -> Context.or_ ctx [ gen_bool s (depth - 1); gen_bool s (depth - 1) ]
    | _ ->
      (match rand_int 2 with
       | 0 -> Context.iff ctx (gen_bool s (depth - 1)) (gen_bool s (depth - 1))
       | _ ->
         Context.ite
           ctx
           (gen_bool s (depth - 1))
           (gen_bool s (depth - 1))
           (gen_bool s (depth - 1))))
;;

(* ------------------------------------------------------------------ *)
(* Enumerate assignments to the free variables (Int over a window, Bool over [{F,T}]) and
   run a check for each. *)

let window_lo = -3
let window_hi = 3
let window_n = window_hi - window_lo + 1

let with_each_assignment s (k : interp -> unit) =
  let total = ref 0 in
  let int_vals = Array.make n_int_vars 0 in
  let bool_vals = Array.make n_bool_vars false in
  let rec loop_int i =
    if i = n_int_vars
    then loop_bool 0
    else
      for v = 0 to window_n - 1 do
        int_vals.(i) <- window_lo + v;
        loop_int (i + 1)
      done
  and loop_bool j =
    if j = n_bool_vars
    then (
      let ints = SymTbl.create 8 in
      let bools = SymTbl.create 8 in
      Array.iteri (fun i sym -> SymTbl.replace ints sym int_vals.(i)) s.x;
      Array.iteri (fun j sym -> SymTbl.replace bools sym bool_vals.(j)) s.p;
      let interp =
        { ints
        ; bools
        ; defs = SymTbl.create 8
        ; div_sym = Env.div_sym s.env
        ; mod_sym = Env.mod_sym s.env
        }
      in
      incr total;
      k interp)
    else
      List.iter
        (fun b ->
           bool_vals.(j) <- b;
           loop_bool (j + 1))
        [ false; true ]
  in
  loop_int 0;
  !total
;;

let load_defs interp (defs : Preprocess.definition list) =
  List.iter
    (fun (d : Preprocess.definition) -> SymTbl.replace interp.defs d.symbol d.value)
    defs
;;

(* ------------------------------------------------------------------ *)
(* Pass equivalence: original vs preprocessed, witnessing fresh symbols via definitions. *)

let assignments_checked = ref 0

let check_equiv s label ~orig ~result ~defs =
  let n =
    with_each_assignment s (fun interp ->
      load_defs interp defs;
      let a = eval_bool interp orig in
      let b = eval_bool interp result in
      if not (Bool.equal a b) then check (Printf.sprintf "equiv %s" label) false)
  in
  assignments_checked := !assignments_checked + n
;;

let pipeline_ok s label t =
  (match Term.Debug.check ~mode:Term.Debug.Pipeline ~env:s.env t with
   | () -> ()
   | exception Failure msg ->
     check (Printf.sprintf "pipeline-check %s: %s" label msg) false);
  check (Printf.sprintf "no-int-ite %s" label) (not (has_int_ite t));
  check (Printf.sprintf "no-divmod %s" label) (not (has_divmod_app t))
;;

let construction_ok label t =
  match Term.Debug.check ~mode:Term.Debug.Construction t with
  | () -> ()
  | exception Failure msg ->
    check (Printf.sprintf "construction-check %s: %s" label msg) false
;;

let n_pass_formulas = 300
let pass_depth = 3

let test_pass_equivalence () =
  let s = make_session () in
  set_seed 0xA5A5A5A5;
  for _ = 1 to n_pass_formulas do
    let phi = gen_bool s pass_depth in
    construction_ok "orig" phi;
    (* div/mod elimination alone *)
    let phi_dm, d_dm = Preprocess.div_mod_elimination s.pp phi in
    construction_ok "divmod" phi_dm;
    check "divmod removes div/mod apps" (not (has_divmod_app phi_dm));
    check_equiv s "divmod" ~orig:phi ~result:phi_dm ~defs:d_dm;
    (* ite removal alone *)
    let phi_ite, d_ite = Preprocess.ite_removal s.pp phi in
    construction_ok "ite" phi_ite;
    check "ite removes int-ite" (not (has_int_ite phi_ite));
    check_equiv s "ite" ~orig:phi ~result:phi_ite ~defs:d_ite;
    (* full pipeline *)
    let phi_run, d_run = Preprocess.run_with_definitions s.pp phi in
    construction_ok "run" phi_run;
    pipeline_ok s "run" phi_run;
    check_equiv s "run" ~orig:phi ~result:phi_run ~defs:d_run
  done
;;

(* ------------------------------------------------------------------ *)
(* Adversarial nests explicitly (ADR-0003 §5): ite in a div argument, div in an ite
   branch, iff chains, mod inside arith inside a comparison. *)

let test_adversarial_nests () =
  let s = make_session () in
  let ctx = s.ctx in
  let x0 = Context.const ctx s.x.(0) in
  let x1 = Context.const ctx s.x.(1) in
  let p0 = Context.const ctx s.p.(0) in
  let p1 = Context.const ctx s.p.(1) in
  let k n = Context.int_const ctx n in
  let cases =
    [ ( "ite-in-div-arg"
      , Context.le ctx (Context.div ctx (Context.ite ctx p0 x0 x1) (k 3)) (k 0) )
    ; ( "div-in-ite-branch"
      , Context.eq ctx (Context.ite ctx p0 (Context.div ctx x0 (k 2)) x1) (k 1) )
    ; ( "mod-in-arith-in-cmp"
      , Context.lt ctx (Context.add ctx x0 (Context.mod_ ctx x1 (k 3))) (k 4) )
    ; ( "iff-chain"
      , Context.iff
          ctx
          (Context.iff ctx p0 (Context.le ctx x0 (k 0)))
          (Context.iff ctx p1 (Context.le ctx x1 (k 0))) )
    ; ( "ite-of-div-and-mod-shared"
      , Context.eq
          ctx
          (Context.div ctx x0 (k 2))
          (Context.ite ctx p0 (Context.mod_ ctx x0 (k 2)) x1) )
    ; ( "nested-int-ite"
      , Context.le ctx (Context.ite ctx p0 (Context.ite ctx p1 x0 x1) (k 5)) (k 0) )
    ]
  in
  List.iter
    (fun (label, phi) ->
       construction_ok label phi;
       let phi_run, d_run = Preprocess.run_with_definitions s.pp phi in
       construction_ok (label ^ "-run") phi_run;
       pipeline_ok s label phi_run;
       check_equiv s label ~orig:phi ~result:phi_run ~defs:d_run)
    cases
;;

(* ------------------------------------------------------------------ *)
(* Clausifier: enumerate Boolean assignments over the original opaque atoms and check
   [original <=> (Tseitin extension satisfies all clauses)]. *)

let collect_atoms (t : Term.t) : Term.t list =
  let seen : unit Term.Table.t = Term.Table.create 64 in
  let acc = ref [] in
  let rec go (t : Term.t) =
    if Term.Table.mem seen t
    then ()
    else (
      Term.Table.add seen t ();
      if Theory_view.is_atom t
      then (
        match t.node with
        | Bool_const _ -> () (* constant: not a free atom *)
        | _ -> acc := t :: !acc)
      else (
        match t.node with
        | Not a -> go a
        | And xs | Or xs -> Iarr.iter go xs
        | Eq (a, b) ->
          go a;
          go b
        | Ite (c, a, b) ->
          go c;
          go a;
          go b
        | _ -> ()))
  in
  go t;
  !acc
;;

(* Evaluate the Boolean skeleton with atoms read from [atomval] (Bool_const uses its own
   value). Reused for both the original formula and each var's defining subterm. *)
let rec eval_skeleton atomval (t : Term.t) : bool =
  if Theory_view.is_atom t
  then (
    match t.node with
    | Bool_const b -> b
    | _ -> atomval t)
  else (
    match t.node with
    | Not a -> not (eval_skeleton atomval a)
    | And xs -> Iarr.for_all (eval_skeleton atomval) xs
    | Or xs -> Iarr.exists (eval_skeleton atomval) xs
    | Eq (a, b) -> Bool.equal (eval_skeleton atomval a) (eval_skeleton atomval b)
    | Ite (c, a, b) ->
      if eval_skeleton atomval c then eval_skeleton atomval a else eval_skeleton atomval b
    | _ -> failwith "eval_skeleton: unexpected node")
;;

let clause_sat var_val (cl : Cnf.Clause.t) =
  List.exists
    (fun lit -> Bool.equal (var_val (Cnf.Lit.var lit)) (Cnf.Lit.is_positive lit))
    cl
;;

(* Converse direction, which the deterministic-extension check above is structurally BLIND
   to: EVERY model of the clause set must project (on the atoms) to a model of the
   original formula. The check above only ever evaluates the ONE canonical extension (each
   aux var := the truth of its defining subterm), which satisfies whatever clauses remain
   — so a *dropped* biconditional direction (an aux var no longer pinned to its subterm)
   is invisible to it. Here we instead search over ALL variables (atoms + aux) and require
   that any full assignment satisfying every clause has an atom projection on which the
   original is true. The Tseitin root unit forces the root literal true, and the
   biconditional clauses force each aux var to equal its subterm, so on a correct
   clausifier this holds; a missing direction admits a clause-model (e.g. a
   negatively-occurring [And] whose var is false while its children are all true) whose
   projection falsifies the original. Exhaustive over [2^num_vars], guarded by a size
   bound (small after hash-consing). *)
let converse_rows_checked = ref 0
let converse_var_bound = 16

let check_cnf_converse label (phi : Term.t) (cnf : Cnf.t) =
  let n = Cnf.num_vars cnf in
  if n <= converse_var_bound
  then (
    let bits = Array.make (n + 1) false in
    let var_val v = bits.(v) in
    (* eval_skeleton only queries atom subterms, each of which is a variable. *)
    let atomval a =
      match Cnf.var_of_atom cnf a with
      | Some v -> bits.(v)
      | None -> false
    in
    let rec loop v =
      if v > n
      then (
        let all_sat = ref true in
        Cnf.iter_clauses
          (fun cl -> if not (clause_sat var_val cl) then all_sat := false)
          cnf;
        incr converse_rows_checked;
        if !all_sat && not (eval_skeleton atomval phi)
        then
          check
            (Printf.sprintf "clausify converse %s (CNF-model => original)" label)
            false)
      else
        List.iter
          (fun b ->
             bits.(v) <- b;
             loop (v + 1))
          [ false; true ]
    in
    loop 1)
;;

let n_clausify_formulas = 400
let clausify_depth = 3
let clauses_emitted = ref 0
let bool_rows_checked = ref 0

let test_clausifier () =
  let s = make_session () in
  set_seed 0x0C0FFEE1;
  for _ = 1 to n_clausify_formulas do
    (* The clausifier's contract is a preprocessed formula (no Int-Ite / div / mod), so
       clausify the pipeline output; treat its atoms as opaque booleans. *)
    let phi = Preprocess.run s.pp (gen_bool s clausify_depth) in
    let cnf = Cnf.clausify phi in
    clauses_emitted := !clauses_emitted + List.length (Cnf.clauses cnf);
    check_cnf_converse "random" phi cnf;
    let atoms = collect_atoms phi in
    let k = List.length atoms in
    if k <= 14
    then (
      let atoms = Array.of_list atoms in
      let bits = Array.make k false in
      let rec loop i =
        if i = k
        then (
          (* atom assignment as a tag-keyed lookup *)
          let assign : bool Term.Table.t = Term.Table.create 32 in
          Array.iteri (fun j a -> Term.Table.replace assign a bits.(j)) atoms;
          let atomval a = Term.Table.find assign a in
          let truth = eval_skeleton atomval phi in
          (* extend to every variable via its defining subterm *)
          let var_val v = eval_skeleton atomval (Cnf.subterm_of_var cnf v) in
          let all_sat =
            let ok = ref true in
            Cnf.iter_clauses
              (fun cl -> if not (clause_sat var_val cl) then ok := false)
              cnf;
            !ok
          in
          incr bool_rows_checked;
          if not (Bool.equal truth all_sat) then check "clausify equivalence" false)
        else
          List.iter
            (fun b ->
               bits.(i) <- b;
               loop (i + 1))
            [ false; true ]
      in
      loop 0)
  done
;;

(* Hand-built formulas where an [And]/[Or]/iff node occurs UNDER A NEGATION, so both
   directions of its Tseitin biconditional are load-bearing. These are the shapes that
   expose a dropped clause direction (which random depth-3 generation may or may not cover
   within the converse size bound), verified exhaustively by [check_cnf_converse]. Each
   stays tiny (few vars), so the converse search is exhaustive. *)
let test_clausifier_negpol () =
  let s = make_session () in
  let ctx = s.ctx in
  let p0 = Context.const ctx s.p.(0) in
  let p1 = Context.const ctx s.p.(1) in
  let x0 = Context.const ctx s.x.(0) in
  let k n = Context.int_const ctx n in
  let le0 = Context.le ctx x0 (k 0) in
  let na = Context.not_ ctx (Context.and_ ctx [ p0; p1 ]) in
  let cases =
    [ "neg-and", na
    ; "neg-and-or", Context.or_ ctx [ na; p0 ]
    ; "neg-and-mixed", Context.not_ ctx (Context.and_ ctx [ p0; le0 ])
    ; "neg-or", Context.not_ ctx (Context.or_ ctx [ p0; p1 ])
    ; "neg-iff", Context.not_ ctx (Context.iff ctx p0 p1)
    ; "ite-neg-and", Context.ite ctx p0 na p1
    ]
  in
  List.iter
    (fun (label, phi) ->
       let phi = Preprocess.run s.pp phi in
       check_cnf_converse label phi (Cnf.clausify phi))
    cases
;;

(* Check the atom map is a faithful bijection on atom variables, and that atom vs
   auxiliary classification matches [Theory_view.is_atom]. *)
let test_atom_map () =
  let s = make_session () in
  set_seed 0xDECAF;
  for _ = 1 to 100 do
    let phi = Preprocess.run s.pp (gen_bool s clausify_depth) in
    let cnf = Cnf.clausify phi in
    for v = 1 to Cnf.num_vars cnf do
      let sub = Cnf.subterm_of_var cnf v in
      if Cnf.is_atom_var cnf v
      then (
        check "atom_of_var some" (Cnf.atom_of_var cnf v <> None);
        check "atom is_atom" (Theory_view.is_atom sub);
        check "var_of_atom roundtrip" (Cnf.var_of_atom cnf sub = Some v))
      else (
        check "aux atom_of_var none" (Cnf.atom_of_var cnf v = None);
        check "aux not is_atom" (not (Theory_view.is_atom sub)))
    done
  done
;;

(* ------------------------------------------------------------------ *)
(* Determinism (I6): identical input built in two fresh sessions yields identical CNF
   (same var count, same DIMACS clauses). Exercises the full pipeline + clausifier. *)

let dimacs cnf = List.map (List.map Cnf.Lit.to_dimacs) (Cnf.clauses cnf)

let build_and_clausify seed =
  let s = make_session () in
  set_seed seed;
  let phi = gen_bool s pass_depth in
  let phi_run = Preprocess.run s.pp phi in
  let cnf = Cnf.clausify phi_run in
  Cnf.num_vars cnf, dimacs cnf
;;

let test_determinism () =
  List.iter
    (fun seed ->
       let a = build_and_clausify seed in
       let b = build_and_clausify seed in
       check "determinism num_vars" (fst a = fst b);
       check "determinism clauses" (snd a = snd b))
    [ 1; 2; 3; 7; 42; 0xBEEF; 0x1234; 99 ];
  (* also: clausify the same term twice in one session is identical *)
  let s = make_session () in
  set_seed 5;
  let phi = Preprocess.run s.pp (gen_bool s pass_depth) in
  check "clausify twice identical" (dimacs (Cnf.clausify phi) = dimacs (Cnf.clausify phi))
;;

(* ------------------------------------------------------------------ *)
(* Negative: div/mod by a non-constant or zero divisor is rejected with [Unsupported] at
   the construction/session boundary BEFORE any interning, so the context table is
   unpolluted (INVARIANTS.md I8). *)

let test_unsupported () =
  let s = make_session () in
  let ctx = s.ctx in
  let x0 = Context.const ctx s.x.(0) in
  let x1 = Context.const ctx s.x.(1) in
  let zero = Context.int_const ctx 0 in
  (* Measure around only the raising calls: all operands already interned, so a state-safe
     Unsupported must leave [term_count] untouched (INVARIANTS.md I8). *)
  let before = Context.term_count ctx in
  check_raises "div by non-constant" (fun () -> Context.div ctx x0 x1);
  check_raises "mod by non-constant" (fun () -> Context.mod_ ctx x0 x1);
  check_raises "div by zero" (fun () -> Context.div ctx x0 zero);
  check_raises "mod by zero" (fun () -> Context.mod_ ctx x0 zero);
  let after = Context.term_count ctx in
  check "context table unpolluted after Unsupported" (before = after)
;;

(* ------------------------------------------------------------------ *)
(* A couple of hand-checked shapes to pin exact behavior. *)

let test_spot_checks () =
  let s = make_session () in
  let ctx = s.ctx in
  let x0 = Context.const ctx s.x.(0) in
  let p0 = Context.const ctx s.p.(0) in
  let k n = Context.int_const ctx n in
  (* A formula with no ite/div/mod: run is a structural no-op (same term). *)
  let plain = Context.le ctx (Context.add ctx x0 (k 1)) (k 0) in
  check "run identity on plain formula" (Term.equal plain (Preprocess.run s.pp plain));
  (* A lone atom clausifies to one var + a unit root clause. *)
  let cnf = Cnf.clausify p0 in
  check "lone atom: one var" (Cnf.num_vars cnf = 1);
  check "lone atom: one clause" (List.length (Cnf.clauses cnf) = 1);
  check "lone atom: atom mapped" (Cnf.atom_of_var cnf 1 = Some p0);
  (* Bool constants clausify to a forcing unit + a root unit; true is sat, false unsat. *)
  let ctrue = Cnf.clausify (Context.bool_const ctx true) in
  let cfalse = Cnf.clausify (Context.bool_const ctx false) in
  check "true: forcing agrees with root" (dimacs ctrue = [ [ 1 ]; [ 1 ] ]);
  check "false: forcing contradicts root" (dimacs cfalse = [ [ -1 ]; [ 1 ] ])
;;

(* ------------------------------------------------------------------ *)
(* Pinned out-of-window div/mod: constants far outside the [-3,3] enumeration window, with
   the euclidean q,r computed by hand. This checks the div/mod encoding on magnitudes the
   random property test above can never reach. *)

let test_pinned_divmod () =
  let s = make_session () in
  let ctx = s.ctx in
  let k n = Context.int_const ctx n in
  let empty_interp () =
    { ints = SymTbl.create 1
    ; bools = SymTbl.create 1
    ; defs = SymTbl.create 4
    ; div_sym = Env.div_sym s.env
    ; mod_sym = Env.mod_sym s.env
    }
  in
  let pinned label a d ~q ~r =
    (* the hand-computed witness must itself satisfy the euclidean spec *)
    check (label ^ " hand-euclid") (a = (d * q) + r && 0 <= r && r < abs d);
    (* div a d = q /\ mod a d = r should preprocess to a formula that evaluates true once
       the fresh q,r are given their witnessed (euclidean) values; flipping q by one must
       evaluate false. *)
    let good =
      Context.and_
        ctx
        [ Context.eq ctx (Context.div ctx (k a) (k d)) (k q)
        ; Context.eq ctx (Context.mod_ ctx (k a) (k d)) (k r)
        ]
    in
    let bad = Context.eq ctx (Context.div ctx (k a) (k d)) (k (q + 1)) in
    let good', dg = Preprocess.run_with_definitions s.pp good in
    let bad', db = Preprocess.run_with_definitions s.pp bad in
    construction_ok (label ^ " good") good';
    pipeline_ok s (label ^ " good") good';
    let ig = empty_interp () in
    load_defs ig dg;
    check (label ^ " good evaluates true") (eval_bool ig good');
    let ib = empty_interp () in
    load_defs ib db;
    check (label ^ " bad evaluates false") (not (eval_bool ib bad'))
  in
  (* 1000003 = 7*142857 + 4 *)
  pinned "div/mod 1000003/7" 1000003 7 ~q:142857 ~r:4;
  (* -999983 = 13*(-76922) + 3 *)
  pinned "div/mod -999983/13" (-999983) 13 ~q:(-76922) ~r:3;
  (* negative divisor: -1000000 = (-3)*333334 + 2 *)
  pinned "div/mod -1000000/-3" (-1000000) (-3) ~q:333334 ~r:2
;;

(* ------------------------------------------------------------------ *)

let () =
  test_pass_equivalence ();
  test_adversarial_nests ();
  test_clausifier ();
  test_clausifier_negpol ();
  test_atom_map ();
  test_determinism ();
  test_unsupported ();
  test_spot_checks ();
  test_pinned_divmod ();
  Printf.printf "preprocess_test: %d checks, %d failures\n" !checks !failures;
  Printf.printf
    "  brute-force: %d pass-equivalence assignments, %d clausifier bool-rows, %d \
     clausifier converse rows, %d clauses emitted\n"
    !assignments_checked
    !bool_rows_checked
    !converse_rows_checked
    !clauses_emitted;
  if !failures > 0 then exit 1
;;
