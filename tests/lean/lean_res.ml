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

(* Does [needle] occur as a substring of [hay]? *)
let find_sub_str (hay : string) (needle : string) : bool =
  let nl = String.length needle
  and hl = String.length hay in
  let rec loop i =
    if i + nl > hl
    then false
    else if String.sub hay i nl = needle
    then true
    else loop (i + 1)
  in
  loop 0
;;

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

(* ---- Rung 3b: EUF theory-leaf discharge (emitter-reconstructed congruence) ---- *)

(* The certificate stores only the leaf CLAUSE for an EUF conflict (no proof chain). The
   emitter therefore reconstructs the congruence argument itself: it renders every EUF
   term over fresh Lean parameters (uninterpreted sorts -> [Type] params with
   [DecidableEq]; function/constant symbols -> value params), runs congruence closure over
   the leaf's equality premises, and emits an explicit [Eq.trans]/[congrArg] proof term
   that the kernel judges. A broken chain fails to typecheck -> loud reject. Anything
   outside the supported fragment (non-equality atom, Real/Array/BitVec sort, arithmetic
   inside an EUF term) makes the whole leaf a trusted hypothesis (honest Valid_modulo). *)
type euf_ctx =
  { sorts : (string, string) Hashtbl.t (* sort key -> Lean type name *)
  ; sort_dec : (string, unit) Hashtbl.t (* type names needing DecidableEq *)
  ; mutable sort_order : string list (* Lean type-param names, first-seen order *)
  ; mutable sort_next : int
  ; syms : (int, string) Hashtbl.t (* Symbol id -> Lean value-param name *)
  ; mutable sym_order : (string * string) list (* (name, Lean type), first-seen order *)
  ; mutable sym_next : int
  }

let euf_ctx_create () =
  { sorts = Hashtbl.create 8
  ; sort_dec = Hashtbl.create 8
  ; sort_order = []
  ; sort_next = 0
  ; syms = Hashtbl.create 16
  ; sym_order = []
  ; sym_next = 0
  }
;;

let sort_key (s : Sort.t) : string =
  match s with
  | Sort.Bool -> "Bool"
  | Sort.Int _ -> "Int"
  | Sort.Uninterpreted sym -> "U:" ^ Oxsmt_core.Symbol.name sym
  | Sort.Datatype sym -> "D:" ^ Oxsmt_core.Symbol.name sym
  | Sort.Real | Sort.Array _ | Sort.BitVec _ -> gapf "EUF: unsupported sort"
;;

(* Lean type name for a sort. Concrete Int/Bool map to themselves; uninterpreted and
   datatype sorts become fresh [Type] parameters. *)
let sort_ty (ctx : euf_ctx) (s : Sort.t) : string =
  let key = sort_key s in
  match key with
  | "Bool" -> "Bool"
  | "Int" -> "Int"
  | _ ->
    (match Hashtbl.find_opt ctx.sorts key with
     | Some n -> n
     | None ->
       let n = Printf.sprintf "T%d" ctx.sort_next in
       ctx.sort_next <- ctx.sort_next + 1;
       Hashtbl.replace ctx.sorts key n;
       ctx.sort_order <- ctx.sort_order @ [ n ];
       n)
;;

(* Mark a sort as needing DecidableEq (it appears as the sort of an equated pair).
   Int/Bool already have it; only uninterpreted/datatype type params get an added instance
   binder. *)
let mark_dec (ctx : euf_ctx) (s : Sort.t) : unit =
  let ty = sort_ty ctx s in
  if ty <> "Int" && ty <> "Bool" then Hashtbl.replace ctx.sort_dec ty ()
;;

(* Render an EUF term as a Lean expression over the context's parameters. Registers each
   symbol's value parameter (constant or function) on first sight. Non-EUF shapes -> Gap. *)
let rec render_term (ctx : euf_ctx) (t : Term.t) : string =
  match t.Term.node with
  | Term.Bool_const b -> if b then "true" else "false"
  | Term.Int_const c -> lean_int c
  | Term.App (sym, args) ->
    let args = Iarr.to_list args in
    let name =
      let id = (sym :> int) in
      match Hashtbl.find_opt ctx.syms id with
      | Some n -> n
      | None ->
        let n = Printf.sprintf "fn%d" ctx.sym_next in
        ctx.sym_next <- ctx.sym_next + 1;
        Hashtbl.replace ctx.syms id n;
        let ty =
          let res = sort_ty ctx t.Term.sort in
          match args with
          | [] -> res
          | _ ->
            let argtys = List.map (fun a -> sort_ty ctx a.Term.sort) args in
            String.concat " -> " (argtys @ [ res ])
        in
        ctx.sym_order <- ctx.sym_order @ [ n, ty ];
        n
    in
    (match args with
     | [] -> name
     | _ ->
       Printf.sprintf "(%s %s)" name (String.concat " " (List.map (render_term ctx) args)))
  | _ -> gapf "EUF: non-congruent term in leaf atom"
;;

(* ---- congruence closure with proof production (per leaf) ---- *)

(* All subterms of [t], deepest-first not required; used to seed CC node set. *)
let rec subterms acc (t : Term.t) =
  let acc = t :: acc in
  match t.Term.node with
  | Term.App (_, args) -> Iarr.to_list args |> List.fold_left subterms acc
  | _ -> acc
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

(* One literal of an EUF leaf: its clause literal, SAT var, rendered equality sides, and
   the Lean type of the equated sort (for the [rhoB] decide binding). *)
type euf_prem =
  { elit : lit
  ; evar : int
  ; elhs : string
  ; erhs : string
  ; ety : string
  }

(* An EUF leaf the emitter can DISCHARGE. [used] are the literal positions whose Prop fact
   the contradiction references; [close] is the position of the disequality it
   contradicts; [eqproof] is the emitter-reconstructed congruence proof that the closing
   disequality's two sides are equal, over the [he_<pos>] equality-fact names. rho is
   substituted in phase B. *)
type euf_discharged =
  { eleaf : lit list
  ; eprems : euf_prem list
  ; used : int list
  ; close : int
  ; eqproof : string
  }

(* Try to discharge an EUF theory leaf: parse each literal's atom as an equality, run
   congruence closure over the equality premises (negative literals), and find a
   disequality (positive literal) whose two sides are forced equal. Any non-equality atom,
   unsupported sort, or non-congruent leaf makes the whole leaf a trusted hypothesis
   (returns None). *)
let try_discharge_euf
  (ctx : euf_ctx)
  ~(resolve_atom : int -> Term.t option)
  (t : Recorder.theory_event)
  : euf_discharged option
  =
  let leaf = clause_of_sat t.Recorder.clause in
  let exception Skip in
  (* per-position: (lit, var, sign, term_a, term_b, rendered_a, rendered_b) *)
  let parse_pos i (v, s) =
    match resolve_atom v with
    | None -> raise Skip
    | Some atom ->
      (match atom.Term.node with
       | Term.Eq (a, b) ->
         let ra = render_term ctx a in
         let rb = render_term ctx b in
         mark_dec ctx a.Term.sort;
         let prem =
           { elit = v, s; evar = v; elhs = ra; erhs = rb; ety = sort_ty ctx a.Term.sort }
         in
         i, (v, s), a, b, prem
       | _ -> raise Skip)
  in
  try
    let parsed = List.mapi parse_pos leaf in
    let eprems = List.map (fun (_, _, _, _, p) -> p) parsed in
    (* CC node set: every subterm of every atom side. *)
    let nodes =
      List.fold_left (fun acc (_, _, a, b, _) -> subterms (subterms acc a) b) [] parsed
    in
    let nodes =
      (* dedup by tag *)
      let seen = Hashtbl.create 64 in
      List.filter
        (fun (n : Term.t) ->
          if Hashtbl.mem seen n.Term.tag
          then false
          else (
            Hashtbl.replace seen n.Term.tag ();
            true))
        nodes
    in
    (* known equalities: (tag_x, tag_y) -> Lean proof of [x = y]; stored both directions. *)
    let known : (int * int, string) Hashtbl.t = Hashtbl.create 64 in
    let get (x : Term.t) (y : Term.t) : string option =
      if x.Term.tag = y.Term.tag
      then Some "rfl"
      else Hashtbl.find_opt known (x.Term.tag, y.Term.tag)
    in
    let add (x : Term.t) (y : Term.t) (pf : string) =
      if x.Term.tag <> y.Term.tag && not (Hashtbl.mem known (x.Term.tag, y.Term.tag))
      then (
        Hashtbl.replace known (x.Term.tag, y.Term.tag) pf;
        Hashtbl.replace known (y.Term.tag, x.Term.tag) (Printf.sprintf "(Eq.symm %s)" pf))
    in
    (* seed with the equality premises (negative literals in the clause). *)
    List.iter
      (fun (i, (_, s), a, b, _) -> if not s then add a b (Printf.sprintf "he_%d" i))
      parsed;
    (* congruence chain proof for [App(_,us) = App(_,ws)] given per-arg proofs. *)
    let cong_proof fname (us : Term.t list) (ws : Term.t list) : string option =
      let rus = List.map (render_term ctx) us in
      let rws = List.map (render_term ctx) ws in
      let n = List.length us in
      let rec build i acc =
        if i = n
        then Some acc
        else (
          match get (List.nth us i) (List.nth ws i) with
          | None -> None
          | Some pi ->
            let slots =
              List.mapi
                (fun j _ ->
                  if j < i then List.nth rws j else if j = i then "z" else List.nth rus j)
                us
            in
            let lam =
              Printf.sprintf "(fun z => (%s %s))" fname (String.concat " " slots)
            in
            let step = Printf.sprintf "(congrArg %s %s)" lam pi in
            let acc =
              if acc = "" then step else Printf.sprintf "(Eq.trans %s %s)" acc step
            in
            build (i + 1) acc)
      in
      build 0 ""
    in
    (* saturate transitivity + congruence to a fixpoint. *)
    let changed = ref true in
    let iters = ref 0 in
    while !changed && !iters < 100 do
      changed := false;
      incr iters;
      (* transitivity *)
      List.iter
        (fun a ->
          List.iter
            (fun b ->
              match get a b with
              | None -> ()
              | Some pab ->
                List.iter
                  (fun c ->
                    match get b c, get a c with
                    | Some pbc, None ->
                      add a c (Printf.sprintf "(Eq.trans %s %s)" pab pbc);
                      changed := true
                    | _ -> ())
                  nodes)
            nodes)
        nodes;
      (* congruence *)
      List.iter
        (fun (u : Term.t) ->
          match u.Term.node with
          | Term.App (fsym, uargs) ->
            let uargs = Iarr.to_list uargs in
            if uargs <> []
            then
              List.iter
                (fun (w : Term.t) ->
                  match w.Term.node with
                  | Term.App (gsym, wargs) ->
                    let wargs = Iarr.to_list wargs in
                    if Oxsmt_core.Symbol.equal fsym gsym
                       && List.length uargs = List.length wargs
                       && get u w = None
                    then (
                      let fname = Hashtbl.find ctx.syms (fsym :> int) in
                      match cong_proof fname uargs wargs with
                      | Some pf ->
                        add u w pf;
                        changed := true
                      | None -> ())
                  | _ -> ())
                nodes
          | _ -> ())
        nodes
    done;
    (* find a disequality (positive literal) whose sides are now congruent. *)
    let closing =
      List.find_map
        (fun (i, (_, s), a, b, _) ->
          if s
          then (
            match get a b with
            | Some pf -> Some (i, pf)
            | None -> None)
          else None)
        parsed
    in
    match closing with
    | None -> None
    | Some (close, eqproof) ->
      (* which he_<pos> names does the proof reference? plus the closing hne. *)
      let used =
        List.filter_map
          (fun (i, (_, s), _, _, _) ->
            if i = close
            then Some i
            else if (not s) && find_sub_str eqproof (Printf.sprintf "he_%d" i)
            then Some i
            else None)
          parsed
      in
      Some { eleaf = leaf; eprems; used; close; eqproof }
  with
  | Skip -> None
;;

(* Emit the full Lean file body: the [rhoF]/[rhoB] theory<->Boolean bridge definitions,
   then [theorem refute] instantiated at [rhoB]. Discharged LIA leaves become proved
   [have]s via {!OxsmtBridge.leaf_sat}; every other input/leaf clause is a trusted
   hypothesis. The res/farkas/bridge preludes are prepended by the driver. *)
let emit_refutation (ev : Checker.events) : string =
  let ai = atom_index_create () in
  let ectx = euf_ctx_create () in
  let atom_tbl = Hashtbl.create 64 in
  List.iter
    (fun (a : Recorder.atom_event) ->
      Hashtbl.replace atom_tbl a.Recorder.var a.Recorder.atom)
    ev.Checker.atoms;
  let resolve_atom v = Hashtbl.find_opt atom_tbl v in
  (* classify theory leaves: LIA Farkas discharge (3b), else EUF congruence discharge,
     else trusted hypothesis (honest Valid_modulo). *)
  let discharged = ref [] in
  let euf_discharged = ref [] in
  let hyp_leaves = ref [] in
  List.iter
    (fun (t : Recorder.theory_event) ->
      let leaf = clause_of_sat t.Recorder.clause in
      let as_hyp () = hyp_leaves := leaf :: !hyp_leaves in
      match t.Recorder.lia_witness with
      | Some w ->
        (match try_discharge ai ~resolve_atom t w with
         | Some d -> discharged := d :: !discharged
         | None -> as_hyp ())
      | None ->
        (match try_discharge_euf ectx ~resolve_atom t with
         | Some d -> euf_discharged := d :: !euf_discharged
         | None | (exception Gap _) -> as_hyp ()))
    ev.Checker.theory;
  let discharged = List.rev !discharged in
  let euf_discharged = List.rev !euf_discharged in
  let hyp_leaves = List.rev !hyp_leaves in
  (* ai / ectx now fully populated. rhoF ranges over LIA int atoms only; rhoB and the
     theorem take a shared parameter prefix: uninterpreted sorts (with DecidableEq),
     function/constant symbols, then the LIA integers, then barb. *)
  let nvars = ai.next in
  let type_names = ectx.sort_order in
  let sym_names = List.map fst ectx.sym_order in
  let int_names = List.init nvars (fun i -> Printf.sprintf "a%d" i) in
  let int_params = String.concat " " int_names in
  let all_arg_names = type_names @ sym_names @ int_names in
  let rhoF_applied =
    if nvars = 0 then "rhoF" else Printf.sprintf "(rhoF %s)" int_params
  in
  let rho =
    match all_arg_names with
    | [] -> "(rhoB barb)"
    | _ -> Printf.sprintf "(rhoB %s barb)" (String.concat " " all_arg_names)
  in
  (* shared binder prefix for [theorem refute] and [def rhoB]. *)
  let prefix_binders =
    let type_bs =
      List.map
        (fun n ->
          if Hashtbl.mem ectx.sort_dec n
          then Printf.sprintf "(%s : Type) [DecidableEq %s]" n n
          else Printf.sprintf "(%s : Type)" n)
        type_names
    in
    let sym_bs =
      List.map (fun (n, ty) -> Printf.sprintf "(%s : %s)" n ty) ectx.sym_order
    in
    let int_bs = if nvars = 0 then [] else [ Printf.sprintf "(%s : Int)" int_params ] in
    String.concat " " (type_bs @ sym_bs @ int_bs)
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
  (* EUF discharged leaves: emitter-reconstructed congruence contradiction as a TERM. *)
  let eprems_arr (d : euf_discharged) = Array.of_list d.eprems in
  List.iter
    (fun (d : euf_discharged) ->
      let arr = eprems_arr d in
      let name = fresh e "hleaf" in
      let bindings =
        List.map
          (fun pos ->
            let p = arr.(pos) in
            let _, s = p.elit in
            let mem_lit = if s then "(true, " else "(false, " in
            let extract =
              Printf.sprintf
                "(OxsmtBridge.lit_false_of_clause_false %s %s %s%d) (by decide) hc)"
                rho
                (render_clause d.eleaf)
                mem_lit
                p.evar
            in
            if s
            then
              Printf.sprintf
                "    let hne_%d := OxsmtBridge.euf_ne_of %s %s %s %d rfl %s"
                pos
                p.elhs
                p.erhs
                rho
                p.evar
                extract
            else
              Printf.sprintf
                "    let he_%d := OxsmtBridge.euf_eq_of %s %s %s %d rfl %s"
                pos
                p.elhs
                p.erhs
                rho
                p.evar
                extract)
          d.used
      in
      Buffer.add_string
        e.buf
        (Printf.sprintf
           "  have %s : satClause %s %s = true :=\n\
           \    OxsmtBridge.euf_leaf_sat %s %s (fun hc =>\n\
            %s\n\
           \    absurd %s hne_%d)\n"
           name
           rho
           (render_clause d.eleaf)
           (render_clause d.eleaf)
           rho
           (String.concat "\n" bindings)
           d.eqproof
           d.close);
      admitted := { name; lits = d.eleaf } :: !admitted)
    euf_discharged;
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
  let sp = if prefix_binders = "" then "" else " " ^ prefix_binders in
  Buffer.add_string
    defs
    (Printf.sprintf
       "def rhoB%s (barb : OxsmtRes.Assign) : OxsmtRes.Assign := fun v =>\n"
       sp);
  (* dedup bindings by var (LIA and EUF atom vars are disjoint SAT variables) *)
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
  List.iter
    (fun (d : euf_discharged) ->
      List.iter
        (fun (p : euf_prem) ->
          if not (Hashtbl.mem seen p.evar)
          then (
            Hashtbl.replace seen p.evar ();
            Buffer.add_string
              defs
              (Printf.sprintf
                 "  if v = %d then decide (%s = %s) else\n"
                 p.evar
                 p.elhs
                 p.erhs)))
        d.eprems)
    euf_discharged;
  Buffer.add_string defs "  barb v\n\n";
  let theorem_params = Printf.sprintf "%s (barb : OxsmtRes.Assign)" sp |> String.trim in
  Printf.sprintf
    "%stheorem refute %s\n%s\n    : False := by\n%s#print axioms refute\n"
    (Buffer.contents defs)
    theorem_params
    (String.concat "\n" hyps)
    (Buffer.contents e.buf)
;;
