(** The instantiation manager (ADR-0012 §1.2): the lemma store, the per-lemma instance
    dedup cache, and — in tranche 1 — a {b manual instance seed queue} standing in for the
    matcher that arrives in tranche 2 ("ships with a trivial matcher (manual-instances
    path)", §8). It lives above the theories and is owned by one {!Session}; it is NOT a
    THEORY and does not touch the frozen seam.

    {b Frame scoping (§1.5).} The store, dedup cache, and seed queue are all keyed by the
    owning frame's selector var: {!on_pop} drops everything a popped frame owned, so a
    lemma and every instance drawn from it retract together (soundness-load-bearing — a
    stranded pushed-frame instance is the C1 wrong-[unsat]).

    {b Budget (§1.4/§3).} A deterministic generation budget, reset per [check_sat] by
    {!begin_check} and debited inside {!round}; on exhaustion the loop degrades to
    [unknown], never hangs. (Tranche 1's manual queue is finite, so the budget rarely
    fires; the shape is in place for tranche 3's generative loops.) *)

open Oxsmt_core
module Sat = Oxsmt_solver.Sat

type t

type stats =
  { live_lemmas : int (* lemmas currently in an active (unpopped) frame *)
  ; instances : int (* ground instances generated so far this session *)
  ; rounds : int (* [round] calls so far this session *)
  ; seeds : int
  (* of [instances], those produced by MBQI-lite seeding (chunk 3) rather than E-matching
     — the population that had no matchable trigger. Session-cumulative. *)
  }

(** Provenance of one generated ground instance (ADR-0012: each instantiation records
    which lemma and which substitution produced it). Certificate replay of these steps is
    a later tranche; the RECORD exists now (see {!instantiations}). *)
type instantiation =
  { lemma_id : int (* the source {!Lemma.t}'s dense id *)
  ; subst : Term.t array (* ground image of each qvar, in [Lemma.qvars] order *)
  ; instance : Term.t (* the resulting ground body [φ[σ]] *)
  }

(** [create ctx env] makes an empty manager over the session's context/env (used to
    rebuild instance bodies and to mint qvars). [gen_budget] caps instances generated per
    [check_sat] (default generous; deterministic, I6).

    Chunk 3 (MBQI-lite seeding). [seed] (default [true]) enables ground-term seeding of
    trigger-inert universals inside {!round}; the session reads [OXSMT_LEMMA_SEED] and
    passes [~seed:false] to build the seeding-disabled mutant (the RED baseline).
    [seed_cap] bounds NEW seed instances per lemma per [check_sat]; [pool_cap] bounds the
    ground-term candidate pool per qvar sort. Both are deterministic and additionally
    clamped by [gen_budget]. *)
val create
  :  ?gen_budget:int
  -> ?seed:bool
  -> ?seed_cap:int
  -> ?pool_cap:int
  -> Context.t
  -> Env.t
  -> t

(** The session's context/env (so the session need not re-thread them). *)
val context : t -> Context.t

val env : t -> Env.t

(** [fresh_id t] allocates the next dense lemma id (used to name the lemma's qvars before
    the body is built, §1.3 mint-before-build). *)
val fresh_id : t -> int

(** [add_lemma t lemma] records [lemma] in the store, live in its [frame] (§1.3). *)
val add_lemma : t -> Lemma.t -> unit

(** [has_live_lemma t] is [true] iff any lemma is in an active (unpopped) frame. THE
    SOUNDNESS RULE (§2): a live lemma degrades a ground [Sat] to [Unknown]. *)
val has_live_lemma : t -> bool

(** [seed_instance t lemma sigma] enqueues a manual instance of [lemma] at substitution
    [sigma] (ground terms in [lemma.qvars] order). {b Tranche-1 scaffold} for the
    manual-instances path (§8): the matcher of tranche 2 replaces this producer, at which
    point {!round} generates its own substitutions and this entry point is retired. *)
val seed_instance : t -> Lemma.t -> Term.t array -> unit

(** [begin_check t] resets the per-[check_sat] generation budget (§1.4 "budget := fresh
    Budget.t"). Does NOT clear the dedup cache or store (those are frame-scoped). *)
val begin_check : t -> unit

(** [round t] produces the next batch of ground instances: in tranche 1 it drains the seed
    queue, applying the dedup filter (skip an instance whose body is already active) and
    debiting the generation budget per instance.

    Tranche 2: [round t view] E-matches every live lemma's triggers against the read-only
    e-graph [view] (deterministic lemma-id order, budget debited INSIDE enumeration, R4)
    AND drains the manual seed queue — both feed one dedup + budget pipeline (a seeded
    instance the matcher also finds dedups).

    Chunk 3 (MBQI-lite): only when E-matching has GLOBALLY saturated this round (no live
    lemma emitted a new instance), [round] seeds each trigger-inert lemma (matcher found
    no substitution — the ground-less Skolem-function population) with existing ground
    terms of each qvar's sort drawn from [view] ({!Egraph_view.ground_terms_by_sort}),
    capped at [seed_cap] NEW instances per lemma per [check_sat]. Gating on global
    saturation (not per-lemma emptiness) confines seeding to genuinely-stuck rounds so it
    does not churn the budget while E-matching is still productive. Every seed instance is
    a ground consequence of the (valid) lemma, so this is universally sound; it feeds the
    same pipeline. Returns [(frame, instance)] pairs; each instance must be asserted
    guarded by its lemma's [frame] selector (§1.4). Deterministic order (matcher output,
    then seed FIFO). [] means saturated (no NEW instance this round). On budget exhaustion
    the round stops early and {!budget_exhausted} is set. *)
val round : t -> Egraph_view.t -> (Sat.var * Instance.t) list

(** [budget_exhausted t] is [true] iff the most recent {!round} stopped on the generation
    budget (→ [unknown], §3). Reset by {!begin_check}. *)
val budget_exhausted : t -> bool

(** [on_pop t selector] drops every lemma, dedup entry, and pending seed owned by the
    frame whose selector is [selector] (§1.5). Called by [Session.pop]. *)
val on_pop : t -> Sat.var -> unit

val stats : t -> stats

(** [instantiations t] is the provenance log, oldest-first (generation order): every
    ground instance actually asserted this session, each tagged with its source lemma id
    and substitution ({!instantiation}). A budget-aborted round's instances are absent
    (they were never asserted); a [pop] does not prune the trace (its soundness is the
    frame selector's job). This is the record the certificate checker will replay in a
    later tranche. *)
val instantiations : t -> instantiation list
