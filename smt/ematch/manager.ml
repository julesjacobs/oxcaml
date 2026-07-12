(* The instantiation manager (ADR-0012 §1.2/§1.4/§1.5). Tranche 1: store + frame-scoped
   liveness/dedup + a manual seed queue + the budget-bounded [round] that drains it. The
   matcher (tranche 2) replaces the seed producer; everything else here is durable. *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat

let default_gen_budget = 100_000

type stats =
  { live_lemmas : int
  ; instances : int
  ; rounds : int
  }

type t =
  { ctx : Context.t
  ; env : Env.t
  ; gen_budget : int
  ; mutable next_id : int
  ; mutable lemmas : Lemma.t list (* the live store; a pop filters it (§1.5) *)
  ; seeds : (Lemma.t * Term.t array) Queue.t (* tranche-1 manual instances *)
  ; dedup : (Sat.var * int, unit) Hashtbl.t
    (* (owning frame selector, instance body tag) -> present. Keyed on (frame, tag), NOT
         tag alone (codex M1): the SAME body guarded by two different selectors is two
         DIFFERENT clauses, so two live lemmas' equal-bodied instances must NOT dedup each
         other. Scoped to active-clause lifetime — dropped on that frame's pop (§1.4 R2). *)
  ; mutable budget_remaining : int
  ; mutable budget_hit : bool
  ; mutable total_instances : int
  ; mutable total_rounds : int
  }

let create ?(gen_budget = default_gen_budget) ctx env =
  { ctx
  ; env
  ; gen_budget
  ; next_id = 0
  ; lemmas = []
  ; seeds = Queue.create ()
  ; dedup = Hashtbl.create 64
  ; budget_remaining = gen_budget
  ; budget_hit = false
  ; total_instances = 0
  ; total_rounds = 0
  }
;;

let context t = t.ctx
let env t = t.env

let fresh_id t =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  id
;;

let add_lemma t (lemma : Lemma.t) = t.lemmas <- lemma :: t.lemmas
let has_live_lemma t = t.lemmas <> []

(* codex C2: only a lemma PHYSICALLY present in this manager's live store may be seeded.
   [List.memq] closes ownership AND liveness in one check: a handle from a different
   session is not in this manager's store (its base selector would otherwise collide,
   riding an active foreign selector -> wrong Unsat); a popped lemma was filtered out of
   the store by [on_pop]. Reject a foreign/stale handle rather than enqueue it. *)
let seed_instance t lemma sigma =
  if not (List.memq lemma t.lemmas)
  then
    invalid_arg
      "Ematch.Manager.seed_instance: lemma handle is not a live lemma of this session \
       (foreign or popped)";
  Queue.add (lemma, sigma) t.seeds
;;

let begin_check t =
  t.budget_remaining <- t.gen_budget;
  t.budget_hit <- false
;;

let budget_exhausted t = t.budget_hit

(* Drain the seed queue, dedup-filtering and budget-debiting per instance (§1.4). Stops on
   budget exhaustion, leaving the rest enqueued (the loop degrades to [unknown] this
   check). Order is seed enqueue order (Queue is FIFO) — deterministic (I6). *)
let round t =
  t.total_rounds <- t.total_rounds + 1;
  let out = ref [] in
  let continue = ref true in
  while !continue && not (Queue.is_empty t.seeds) do
    if t.budget_remaining <= 0
    then (
      t.budget_hit <- true;
      continue := false)
    else (
      let lemma, sigma = Queue.pop t.seeds in
      let inst =
        Instance.of_subst t.ctx ~qvars:lemma.Lemma.qvars ~body:lemma.Lemma.body sigma
      in
      let key = lemma.Lemma.frame, (Instance.to_term inst).Term.tag in
      if Hashtbl.mem t.dedup key
      then () (* redundancy filter: this (frame, body) clause already active (§L5) *)
      else (
        Hashtbl.replace t.dedup key ();
        t.budget_remaining <- t.budget_remaining - 1;
        t.total_instances <- t.total_instances + 1;
        out := (lemma.Lemma.frame, inst) :: !out))
  done;
  List.rev !out
;;

let on_pop t selector =
  t.lemmas <- List.filter (fun (l : Lemma.t) -> not (Int.equal l.frame selector)) t.lemmas;
  (* drop dedup entries owned by the popped frame (§1.4 R2: retracted instance
     re-generates) — the key is (frame, tag), so match on the frame component *)
  let stale =
    Hashtbl.fold
      (fun ((fr, _tag) as key) () acc ->
         if Int.equal fr selector then key :: acc else acc)
      t.dedup
      []
  in
  List.iter (Hashtbl.remove t.dedup) stale;
  (* drop pending seeds whose lemma lived in the popped frame *)
  let kept = Queue.create () in
  Queue.iter
    (fun ((l, _) as s) -> if not (Int.equal l.Lemma.frame selector) then Queue.add s kept)
    t.seeds;
  Queue.clear t.seeds;
  Queue.transfer kept t.seeds
;;

let stats t =
  { live_lemmas = List.length t.lemmas
  ; instances = t.total_instances
  ; rounds = t.total_rounds
  }
;;
