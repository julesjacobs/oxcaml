(* UNSAT propositional resolution skeleton -> Lean (lean-proofs lane, Rung 3a).

   Consumes a recorded {!Oxsmt_certificate.Checker.events} stream for an [Unsat] solve and
   emits ONE self-contained core-Lean-4 proof of [False] over {!res_prelude} ([resolve],
   [sat_mono], [empty_absurd]). The proof's hypotheses are the query input clauses and the
   materialized theory-leaf clauses (in Rung 3a the theory leaves are TRUSTED hypotheses —
   the Boolean refutation modulo unchecked theory steps, mirroring the OCaml checker's
   [Valid_modulo_unchecked_steps]; Rung 3b discharges them with Farkas/EUF/DT proofs).

   Every resolution step comes from the certificate/replay, NEVER from Lean search:
   - each learned clause is re-derived by replaying its recorded ordered-RUP antecedent
     chain as explicit [resolve] steps (the emitter runs the propagation to pick pivots;
     Lean only CHECKS each [resolve]);
   - the terminal [] is reached by an emitter-run unit-propagation refutation over the
     admitted clauses (the recorded conclusion id is advisory — the ground truth, exactly
     as in the OCaml checker, is UP-derivability of [] from the admitted axioms + verified
     learned clauses).

   SOUNDNESS is the kernel's: [resolve]/[sat_mono]/[empty_absurd] are proved once in
   res_prelude; a wrong pivot or dropped premise makes a [resolve] a no-op / an [erase]
   not cancel, so the final clause fails to reduce to [] (or the [sat_mono] subset
   [by decide] fails) and the kernel REJECTS. A bug here can only fail loud, never accept
   a wrong proof.

   A construct/shape this rung cannot replay is a loud {!Gap} (UNSUPPORTED), never a fake
   or weakened obligation. *)

module Sat = Oxsmt_solver.Sat
module Recorder = Oxsmt_certificate.Recorder
module Checker = Oxsmt_certificate.Checker
module Term = Oxsmt_core.Term
module Sort = Oxsmt_core.Sort
module Bigint = Oxsmt_core.Bigint
module Iarr = Oxsmt_core.Iarr
module Rational = Oxsmt_lia.Rational

exception Gap of string

let gapf fmt = Printf.ksprintf (fun s -> raise (Gap s)) fmt

(* A literal as (var, sign): sign=true is the positive literal v, false is ¬v. *)
type lit = int * bool

let lit_of_sat (l : Sat.lit) : lit = Sat.var_of_lit l, Sat.sign_of_lit l
let clause_of_sat (c : Sat.lit array) : lit list = Array.to_list c |> List.map lit_of_sat

(* Lean rendering. Lit (v,true) -> "(true, v)"; sign uses Lean bool literals. *)
let render_lit ((v, s) : lit) : string = Printf.sprintf "(%b, %d)" s v

let render_clause (c : lit list) : string =
  "[" ^ String.concat ", " (List.map render_lit c) ^ "]"
;;

(* ---- Rung 3b: LIA theory-leaf discharge (theory<->Boolean bridge) ---- *)

(* A global map from an atomic arithmetic term (an uninterpreted Int/element const or
   application) to a reflective variable index, shared by every discharged leaf in the
   file so one [rhoF] serves all of them. First-seen order. *)
type atom_index =
  { tbl : (Term.t, int) Hashtbl.t
  ; mutable next : int
  }

let atom_index_create () = { tbl = Hashtbl.create 32; next = 0 }

let atom_idx (ai : atom_index) (t : Term.t) : int =
  match Hashtbl.find_opt ai.tbl t with
  | Some i -> i
  | None ->
    let i = ai.next in
    Hashtbl.replace ai.tbl t i;
    ai.next <- i + 1;
    i
;;

(* Linear form of an Int-sorted term: (coeff, atomic-term) pairs + a constant. Mirrors the
   Farkas leaf emitter's [linear_of]. *)
type linform =
  { terms : (Term.t * Bigint.t) list
  ; const : Bigint.t
  }

let linear_of (t : Term.t) : linform =
  match t.Term.node with
  | Term.Arith { coeffs; const } -> { terms = Iarr.to_list coeffs; const }
  | Term.Int_const c -> { terms = []; const = c }
  | _ -> { terms = [ t, Bigint.one ]; const = Bigint.zero }
;;

let lean_int b = Printf.sprintf "(%s : Int)" (Bigint.to_string b)

(* Render a linform as a reflective [LinExpr] literal over the global atom index. *)
let render_reflrow (ai : atom_index) (f : linform) : string =
  let ts =
    List.map
      (fun (t, c) -> Printf.sprintf "(%s, %d)" (lean_int c) (atom_idx ai t))
      f.terms
  in
  Printf.sprintf "([%s], %s)" (String.concat ", " ts) (lean_int f.const)
;;

(* A proof handle: the Lean identifier that proves [satClause rho <lits> = true]. *)
type handle =
  { name : string
  ; lits : lit list
  }

(* ---- emission buffer / fresh names ---- *)

type emitter =
  { buf : Buffer.t
  ; mutable counter : int
  ; rho : string (* the Lean expression for the Boolean assignment used everywhere *)
  }

let fresh e prefix =
  let n = e.counter in
  e.counter <- n + 1;
  Printf.sprintf "%s%d" prefix n
;;

(* Resolve the running clause D (proved by [dh], whose var [v] literal has sign [dsign])
   against another clause proved by [oh] (its var [v] literal has sign [not dsign]) on
   pivot [v]. Returns the raw resolve EXPRESSION (clause args are holes inferred from the
   two proof terms). The positive-[v] side is c1, the negative-[v] side is c2. *)
let resolve_expr ~rho ~pivot ~dsign ~(dexpr : string) ~(oexpr : string) : string =
  if dsign
  then Printf.sprintf "(resolve %s %d _ _ %s %s)" rho pivot dexpr oexpr
  else Printf.sprintf "(resolve %s %d _ _ %s %s)" rho pivot oexpr dexpr
;;

(* ---- trail-based propagation helpers ---- *)

(* Evaluate a clause under [trail] (var -> bool assignment). Returns:
   - `Satisfied if some literal is true;
   - `Conflict if every literal is false;
   - `Unit l if exactly one literal is unassigned and the rest are false;
   - `Free if two or more literals are unassigned. *)
let eval_clause (trail : (int, bool) Hashtbl.t) (c : lit list) =
  let free = ref [] in
  let satisfied = ref false in
  List.iter
    (fun (v, s) ->
      match Hashtbl.find_opt trail v with
      | Some b when b = s -> satisfied := true
      | Some _ -> () (* falsified *)
      | None -> free := (v, s) :: !free)
    c;
  if !satisfied
  then `Satisfied
  else (
    match !free with
    | [] -> `Conflict
    | [ l ] -> `Unit l
    | _ -> `Free)
;;

(* Emit resolves that remove each [(lit, handle)] in [removals] from the clause proved by
   [base_name] (the removed literal has the recorded sign in the running clause; its
   handle proves the negation), then state the reduced clause as [target] via [sat_mono]
   (a ground [by decide] subset check). Returns a handle proving
   [satClause rho target = true]. *)
let emit_resolve_out
  e
  ~prefix
  ~(base_name : string)
  ~(removals : (lit * handle) list)
  ~(target : lit list)
  : handle
  =
  let expr =
    List.fold_left
      (fun dexpr ((v, s), (uh : handle)) ->
        resolve_expr ~rho:e.rho ~pivot:v ~dsign:s ~dexpr ~oexpr:uh.name)
      base_name
      removals
  in
  let name = fresh e prefix in
  Buffer.add_string
    e.buf
    (Printf.sprintf
       "  have %s : satClause %s %s = true := sat_mono %s _ %s (by decide) %s\n"
       name
       e.rho
       (render_clause target)
       e.rho
       (render_clause target)
       expr);
  { name; lits = target }
;;

(* Refute [clauses] ∪ [{¬ℓ : ℓ ∈ [assumed_false]}] by unit propagation, emitting explicit
   resolutions, and return a handle proving [satClause rho target = true].

   [assumed_false] are literals ASSUMED FALSE (their negations seeded true, with NO proof
   — they are the residual that survives into [target]). For deriving a learned clause L,
   [assumed_false] = [target] = L: BCP under ¬L reaches a conflict (a 1UIP learned clause
   is RUP by construction), and the falsified assumed literals accumulate back into L. For
   the terminal step, [assumed_false] = [target] = [] (a plain UP refutation to []).

   The recorded ordered-RUP antecedents are NOT consulted: the emitter reruns propagation
   itself. This mirrors the OCaml checker's principle that the cited chain is advisory and
   the ground truth is UP-derivability of ⊥. A non-RUP shape is a loud {!Gap}. *)
let refute_under
  e
  ~(clauses : handle list)
  ~(assumed_false : lit list)
  ~(target : lit list)
  : handle
  =
  let trail : (int, bool) Hashtbl.t = Hashtbl.create 64 in
  let unit_handle : (int, handle) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (v, s) -> Hashtbl.replace trail v (not s)) assumed_false;
  (* Split a fully-decided clause's literals into resolvable removals (falsified by a real
     propagated unit, paired with its handle) and the residual (falsified by an
     assumption, or the kept free literal) — the residual is [⊆ target]. *)
  let split_falsified (lits : lit list) ~(keep : lit option) =
    List.fold_right
      (fun (v, s) (removals, residual) ->
        match keep with
        | Some k when (v, s) = k -> removals, (v, s) :: residual
        | _ ->
          (match Hashtbl.find_opt unit_handle v with
           | Some uh -> ((v, s), uh) :: removals, residual
           | None -> removals, (v, s) :: residual))
      lits
      ([], [])
  in
  let pending = ref clauses in
  let result = ref None in
  let progress = ref true in
  while !progress && !result = None do
    progress := false;
    let still = ref [] in
    List.iter
      (fun (h : handle) ->
        if !result <> None
        then still := h :: !still
        else (
          match eval_clause trail h.lits with
          | `Satisfied -> progress := true
          | `Conflict ->
            let removals, _ = split_falsified h.lits ~keep:None in
            let bot =
              emit_resolve_out e ~prefix:"bot" ~base_name:h.name ~removals ~target
            in
            result := Some bot;
            progress := true
          | `Unit (v, s) ->
            let removals, residual0 = split_falsified h.lits ~keep:(Some (v, s)) in
            (* Each removal handle may carry "baggage": assumed literals it accumulated
               when it was itself derived. Resolving on the pivot keeps that baggage, so
               it flows into this clause too — the stated residual must include it (still
               ⊆ target, since every baggage literal is an assumed one). *)
            let baggage =
              List.concat_map
                (fun ((pv, _), (uh : handle)) ->
                  List.filter (fun (vv, _) -> vv <> pv) uh.lits)
                removals
            in
            let residual = List.sort_uniq compare (residual0 @ baggage) in
            let uh =
              emit_resolve_out e ~prefix:"u" ~base_name:h.name ~removals ~target:residual
            in
            Hashtbl.replace trail v s;
            Hashtbl.replace unit_handle v uh;
            progress := true
          | `Free -> still := h :: !still))
      !pending;
    pending := List.rev !still
  done;
  match !result with
  | Some h -> h
  | None -> gapf "not unit-propagation refutable under the given assumptions"
;;

(* ---- top-level ---- *)

(* A LIA theory leaf the emitter can DISCHARGE (Rung 3b): its recorded clause, the Lean
   [prems] list for [OxsmtBridge.leaf_sat], and the [(satvar, reflrow)] bindings [rhoB]
   needs (each satvar mapped to [decide (eval reflrow rhoF ≤ 0)]). *)
(* A premise's kind determines its Farkas ≤0 row, its [rhoB] decide body, and which bridge
   helper discharges it. Row strings are reflective [LinExpr] literals over [ai]. *)
type pkind =
  | Pos of string (* positive Le: atom row r = farkas row; rhoB v = decide (eval r ≤ 0) *)
  | Neg of string (* negative Le: atom row r; farkas row = negbump r *)
  | Eqk of string * string (* equality a=b: rows a,b; farkas row = subL a b *)

type dprem =
  { lit : lit
  ; var : int
  ; mult : Bigint.t
  ; kind : pkind
  }

type discharged =
  { leaf : lit list
  ; prems : dprem list
  }

(* Try to build a discharge for a LIA Farkas leaf. Handles positive [Le], negative [Le]
   (integer-strengthened cut), and positive-multiplier equality premises; anything else
   (disequality, negative-multiplier equality, non-Int, missing atom) makes the whole leaf
   a trusted hypothesis (honest Valid_modulo). Rational multipliers are cleared to
   integers (Farkas is scale-invariant). *)
let try_discharge
  (ai : atom_index)
  ~(resolve_atom : int -> Term.t option)
  (t : Recorder.theory_event)
  (w : Recorder.lia_conflict_witness)
  : discharged option
  =
  let premlist = w.Recorder.premises in
  if premlist = []
  then None
  else (
    let dens =
      List.map
        (fun (p : Recorder.lia_premise) -> Rational.den_bigint p.Recorder.multiplier)
        premlist
    in
    let prod_but i =
      List.fold_left
        (fun acc (j, d) -> if j = i then acc else Bigint.mul acc d)
        Bigint.one
        (List.mapi (fun j d -> j, d) dens)
    in
    let exception Skip in
    try
      (* var -> (mult, kind), built for each premise. *)
      let info = Hashtbl.create 8 in
      List.iteri
        (fun i (p : Recorder.lia_premise) ->
          let polarity = Sat.sign_of_lit p.Recorder.lit in
          let v = Sat.var_of_lit p.Recorder.lit in
          let m = Bigint.mul (Rational.num_bigint p.Recorder.multiplier) (prod_but i) in
          match resolve_atom v with
          | None -> raise Skip
          | Some atom ->
            (match atom.Term.node with
             | Term.Le arg ->
               if not (Sort.equal arg.Term.sort Sort.int) then raise Skip;
               if Bigint.compare m Bigint.zero < 0 then raise Skip;
               let row = render_reflrow ai (linear_of arg) in
               Hashtbl.replace info v (m, if polarity then Pos row else Neg row)
             | Term.Eq (a, b) ->
               if not polarity then raise Skip;
               if not (Sort.equal a.Term.sort Sort.int) then raise Skip;
               if Bigint.compare m Bigint.zero < 0 then raise Skip;
               let ra = render_reflrow ai (linear_of a) in
               let rb = render_reflrow ai (linear_of b) in
               Hashtbl.replace info v (m, Eqk (ra, rb))
             | _ -> raise Skip))
        premlist;
      (* order by the recorded leaf clause; each literal must be the NEGATION of a premise
         we have an entry for (positive premise -> negative leaf lit, and vice versa). *)
      let leaf = clause_of_sat t.Recorder.clause in
      let prems =
        List.map
          (fun (v, s) ->
            match Hashtbl.find_opt info v with
            | Some (m, kind) ->
              (* the leaf literal must be the negation of the premise literal: a Pos/Eqk
                 premise is positive so its leaf lit is negative (s=false); a Neg
                 premise's leaf lit is positive (s=true). *)
              let expect_sign =
                match kind with
                | Neg _ -> true
                | Pos _ | Eqk _ -> false
              in
              if s <> expect_sign then raise Skip;
              { lit = v, s; var = v; mult = m; kind }
            | None -> raise Skip)
          leaf
      in
      Some { leaf; prems }
    with
    | Skip -> None)
;;

(* Emit the full Lean file body: the [rhoF]/[rhoB] theory<->Boolean bridge definitions,
   then [theorem refute] instantiated at [rhoB]. Discharged LIA leaves become proved
   [have]s via {!OxsmtBridge.leaf_sat}; every other input/leaf clause is a trusted
   hypothesis. The res/farkas/bridge preludes are prepended by the driver. *)
let emit_refutation (ev : Checker.events) : string =
  let ai = atom_index_create () in
  let atom_tbl = Hashtbl.create 64 in
  List.iter
    (fun (a : Recorder.atom_event) ->
      Hashtbl.replace atom_tbl a.Recorder.var a.Recorder.atom)
    ev.Checker.atoms;
  let resolve_atom v = Hashtbl.find_opt atom_tbl v in
  (* classify theory leaves into discharged (3b) vs trusted hypothesis *)
  let discharged = ref [] in
  let hyp_leaves = ref [] in
  List.iter
    (fun (t : Recorder.theory_event) ->
      let leaf = clause_of_sat t.Recorder.clause in
      match t.Recorder.lia_witness with
      | Some w ->
        (match try_discharge ai ~resolve_atom t w with
         | Some d -> discharged := d :: !discharged
         | None -> hyp_leaves := leaf :: !hyp_leaves)
      | None -> hyp_leaves := leaf :: !hyp_leaves)
    ev.Checker.theory;
  let discharged = List.rev !discharged in
  let hyp_leaves = List.rev !hyp_leaves in
  (* ai is now fully populated; build rhoF over its indices and rhoB over the discharged
     leaves' bindings. *)
  let nvars = ai.next in
  let int_params =
    List.init nvars (fun i -> Printf.sprintf "a%d" i) |> String.concat " "
  in
  let rhoF_applied =
    if nvars = 0 then "rhoF" else Printf.sprintf "(rhoF %s)" int_params
  in
  let rho =
    if nvars = 0 then "(rhoB barb)" else Printf.sprintf "(rhoB %s barb)" int_params
  in
  let e = { buf = Buffer.create 4096; counter = 0; rho } in
  (* input + trusted-leaf hypotheses *)
  let hyps = ref [] in
  let admitted = ref [] in
  let hidx = ref 0 in
  let add_hyp lits =
    let name = Printf.sprintf "hax%d" !hidx in
    incr hidx;
    hyps
    := Printf.sprintf "  (%s : satClause %s %s = true)" name rho (render_clause lits)
       :: !hyps;
    admitted := { name; lits } :: !admitted
  in
  List.iter
    (fun (i : Recorder.input_event) -> add_hyp (clause_of_sat i.Recorder.clause))
    ev.Checker.inputs;
  List.iter (fun leaf -> add_hyp leaf) hyp_leaves;
  let hyps = List.rev !hyps in
  (* per-premise Lean pieces (depend on rhoF_applied / rho, now fixed). *)
  let farkas_row_of = function
    | Pos r -> r
    | Neg r -> Printf.sprintf "(OxsmtBridge.negbump %s)" r
    | Eqk (a, b) -> Printf.sprintf "(OxsmtBridge.subL %s %s)" a b
  in
  let bind_body_of = function
    | Pos r | Neg r -> Printf.sprintf "decide (OxsmtFarkas.eval %s %s ≤ 0)" r rhoF_applied
    | Eqk (a, b) ->
      Printf.sprintf
        "decide (OxsmtFarkas.eval %s %s = OxsmtFarkas.eval %s %s)"
        a
        rhoF_applied
        b
        rhoF_applied
  in
  (* hlink head: a TERM proving [satLit rho ℓ = false → eval row rho ≤ 0] via the matching
     bridge helper (its [rhoB v = decide …] side condition is [rfl]). *)
  let head_of (p : dprem) =
    match p.kind with
    | Pos r ->
      Printf.sprintf
        "(fun h => OxsmtBridge.prem_pos %s %s %s %d rfl h)"
        r
        rhoF_applied
        rho
        p.var
    | Neg r ->
      Printf.sprintf
        "(fun h => OxsmtBridge.prem_neg %s %s %s %d rfl h)"
        r
        rhoF_applied
        rho
        p.var
    | Eqk (a, b) ->
      Printf.sprintf
        "(fun h => OxsmtBridge.prem_eq %s %s %s %s %d rfl h)"
        a
        b
        rhoF_applied
        rho
        p.var
  in
  let prems_lean (d : discharged) =
    "["
    ^ String.concat
        ", "
        (List.map
           (fun (p : dprem) ->
             Printf.sprintf
               "(%s, (%s, %s))"
               (render_lit p.lit)
               (lean_int p.mult)
               (farkas_row_of p.kind))
           d.prems)
    ^ "]"
  in
  (* hlink : ∀ p ∈ prems, satLit rho p.1 = false → eval p.2.2 rho ≤ 0, as a NESTED
     [List.forall_mem_cons] term (no tactic; guard-safe). *)
  let rec hlink_of = function
    | [] -> "(List.forall_mem_nil _)"
    | p :: rest ->
      Printf.sprintf "(List.forall_mem_cons.mpr ⟨%s, %s⟩)" (head_of p) (hlink_of rest)
  in
  (* body: discharged-leaf bridge haves first (usable by the skeleton) *)
  List.iter
    (fun (d : discharged) ->
      let name = fresh e "hleaf" in
      Buffer.add_string
        e.buf
        (Printf.sprintf
           "  have %s : satClause %s %s = true :=\n\
           \    OxsmtBridge.leaf_sat %s %s %s\n\
           \      %s\n\
           \      (by decide) (by decide) (by decide)\n"
           name
           rho
           (render_clause d.leaf)
           (prems_lean d)
           rho
           rhoF_applied
           (hlink_of d.prems));
      admitted := { name; lits = d.leaf } :: !admitted)
    discharged;
  let admitted0 = List.rev !admitted in
  (* Derive each learned clause in event order (RUP under its own negation over axioms +
     discharged leaves + earlier-derived learned clauses). *)
  let derived = ref [] in
  List.iter
    (fun (l : Recorder.learned_event) ->
      let learned_lits = clause_of_sat l.Recorder.clause in
      let clauses = admitted0 @ List.rev !derived in
      let h = refute_under e ~clauses ~assumed_false:learned_lits ~target:learned_lits in
      derived := h :: !derived)
    ev.Checker.learned;
  let all_clauses = admitted0 @ List.rev !derived in
  let bot = refute_under e ~clauses:all_clauses ~assumed_false:[] ~target:[] in
  Buffer.add_string e.buf (Printf.sprintf "  exact empty_absurd %s %s\n" rho bot.name);
  (* rhoF / rhoB definitions *)
  let defs = Buffer.create 1024 in
  Buffer.add_string
    defs
    "open OxsmtRes\nopen OxsmtFarkas\n\nset_option maxRecDepth 10000\n\n";
  let rhoF_params =
    if nvars = 0
    then ""
    else
      Printf.sprintf
        " (%s : Int)"
        (String.concat " " (List.init nvars (fun i -> Printf.sprintf "a%d" i)))
  in
  Buffer.add_string
    defs
    (Printf.sprintf "def rhoF%s : OxsmtFarkas.Assign := fun n =>\n" rhoF_params);
  if nvars = 0
  then Buffer.add_string defs "  0\n"
  else (
    List.init nvars (fun i -> i)
    |> List.iter (fun i ->
      Buffer.add_string defs (Printf.sprintf "  if n = %d then a%d else\n" i i));
    Buffer.add_string defs "  0\n");
  Buffer.add_string defs "\n";
  let rhoB_params =
    if nvars = 0
    then ""
    else
      Printf.sprintf
        " (%s : Int)"
        (String.concat " " (List.init nvars (fun i -> Printf.sprintf "a%d" i)))
  in
  Buffer.add_string
    defs
    (Printf.sprintf
       "def rhoB%s (barb : OxsmtRes.Assign) : OxsmtRes.Assign := fun v =>\n"
       rhoB_params);
  (* dedup bindings by var *)
  let seen = Hashtbl.create 16 in
  List.iter
    (fun (d : discharged) ->
      List.iter
        (fun (p : dprem) ->
          if not (Hashtbl.mem seen p.var)
          then (
            Hashtbl.replace seen p.var ();
            Buffer.add_string
              defs
              (Printf.sprintf "  if v = %d then %s else\n" p.var (bind_body_of p.kind))))
        d.prems)
    discharged;
  Buffer.add_string defs "  barb v\n\n";
  let theorem_params =
    if nvars = 0
    then "(barb : OxsmtRes.Assign)"
    else Printf.sprintf "(%s : Int) (barb : OxsmtRes.Assign)" int_params
  in
  Printf.sprintf
    "%stheorem refute %s\n%s\n    : False := by\n%s#print axioms refute\n"
    (Buffer.contents defs)
    theorem_params
    (String.concat "\n" hyps)
    (Buffer.contents e.buf)
;;
