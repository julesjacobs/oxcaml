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

(* A proof handle: the Lean identifier that proves [satClause rho <lits> = true]. *)
type handle =
  { name : string
  ; lits : lit list
  }

(* ---- emission buffer / fresh names ---- *)

type emitter =
  { buf : Buffer.t
  ; mutable counter : int
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
let resolve_expr ~pivot ~dsign ~(dexpr : string) ~(oexpr : string) : string =
  if dsign
  then Printf.sprintf "(resolve rho %d _ _ %s %s)" pivot dexpr oexpr
  else Printf.sprintf "(resolve rho %d _ _ %s %s)" pivot oexpr dexpr
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
        resolve_expr ~pivot:v ~dsign:s ~dexpr ~oexpr:uh.name)
      base_name
      removals
  in
  let name = fresh e prefix in
  Buffer.add_string
    e.buf
    (Printf.sprintf
       "  have %s : satClause rho %s = true := sat_mono rho _ %s (by decide) %s\n"
       name
       (render_clause target)
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

(* Build the admitted-clause hypotheses (query + theory-lemma inputs, and theory leaves).
   Returns (hyp declarations, ordered admitted handles). In Rung 3a every theory leaf is a
   TRUSTED hypothesis — the Boolean refutation modulo unchecked theory steps. *)
let build_axioms (ev : Checker.events) : string list * handle list =
  let hyps = ref [] in
  let admitted = ref [] in
  let idx = ref 0 in
  let add_hyp ~lits =
    let name = Printf.sprintf "hax%d" !idx in
    incr idx;
    hyps
    := Printf.sprintf "  (%s : satClause rho %s = true)" name (render_clause lits)
       :: !hyps;
    admitted := { name; lits } :: !admitted
  in
  List.iter
    (fun (i : Recorder.input_event) -> add_hyp ~lits:(clause_of_sat i.Recorder.clause))
    ev.Checker.inputs;
  List.iter
    (fun (t : Recorder.theory_event) -> add_hyp ~lits:(clause_of_sat t.Recorder.clause))
    ev.Checker.theory;
  List.rev !hyps, List.rev !admitted
;;

(* Emit the full Lean file body (theorem [refute] + axiom print). The res_prelude text is
   prepended by the driver, which also opens [OxsmtRes]. *)
let emit_refutation (ev : Checker.events) : string =
  let hyps, admitted = build_axioms ev in
  let e = { buf = Buffer.create 4096; counter = 0 } in
  (* Derive each learned clause in event order (RUP under its own negation over the
     axioms + earlier-derived learned clauses), growing the usable clause pool. *)
  let derived = ref [] in
  List.iter
    (fun (l : Recorder.learned_event) ->
      let learned_lits = clause_of_sat l.Recorder.clause in
      let clauses = admitted @ List.rev !derived in
      let h = refute_under e ~clauses ~assumed_false:learned_lits ~target:learned_lits in
      derived := h :: !derived)
    ev.Checker.learned;
  let all_clauses = admitted @ List.rev !derived in
  let bot = refute_under e ~clauses:all_clauses ~assumed_false:[] ~target:[] in
  Buffer.add_string e.buf (Printf.sprintf "  exact empty_absurd rho %s\n" bot.name);
  let header =
    "open OxsmtRes\n\nset_option maxRecDepth 10000\n\ntheorem refute (rho : Assign)\n"
  in
  Printf.sprintf
    "%s%s\n    : False := by\n%s#print axioms refute\n"
    header
    (String.concat "\n" hyps)
    (Buffer.contents e.buf)
;;
