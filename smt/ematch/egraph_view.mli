(** A read-only view of the solver's congruence closure (ADR-0012 L2 / O3), exposed to the
    E-matcher (tranche 2). It is a record of {b non-registering} closures over
    core-vocabulary types only ([Term.t]/[Symbol.t]/[int]) — so [smt/ematch] depends on
    [core]+[solver] only and never on [oxsmt_euf] (I3). {!Oxsmt_interface} builds a live
    view over the concrete EUF adapter; tests build a hand-rolled one, exactly as the
    combinator is tested against hand-rolled children.

    Every closure is genuinely read-only: the matcher cannot perturb the e-graph, which
    the failure-direction analysis (ADR-0012 §3, R6) requires. An {e unregistered} term is
    a singleton class matched by tag-equality only, so a stale/missing class yields either
    a valid universal instance or no instance, never a wrong refutation (§3 M3). *)

open Oxsmt_core

type t =
  { app_terms_by_symbol : Symbol.t -> Term.t list
    (** Registered ground [App] terms with a given head, in registration order — trigger
      root candidates (R-EM3). *)
  ; find_class_opt : Term.t -> int option
    (** The term's class root iff registered, else [None] (no registration). *)
  ; equal_if_registered : Term.t -> Term.t -> bool
    (** Congruence-equality; an unregistered term is its own singleton class (tag equality). *)
  ; class_members : Term.t -> Term.t list
    (** Members of the term's congruence class in id order; an unregistered term is the
      singleton [[term]]. *)
  }

(** An empty view (no registered term): [app_terms_by_symbol]/[class_members] behave as if
    every queried term is an unregistered singleton, [find_class_opt] is always [None],
    and [equal_if_registered] falls back to [Term.equal]. For tests / a session with no
    theory atoms. The matcher over this view generates no instances. *)
val empty : t
