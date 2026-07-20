(** How the solver reads a term for theory dispatch (ADR-0003 Decision 2). The [App] vs
    arithmetic/[Le] split is the load-bearing signal: EUF congruence-closes only [App];
    [Arith], [Real_arith], and [Le] are opaque leaves owned by LIA or LRA according to
    their sort. *)

type atom =
  | Equality of Term.t * Term.t (* non-Bool Eq: uninterpreted / shared equality *)
  | Le_zero of Term.t (* LIA/LRA: [term <= 0] *)
  | Predicate of Symbol.t * Term.t Iarr.t (* Bool-codomain App *)
  | Bool_lit of bool

(** [is_atom t] is the frozen Decision-2 predicate: for a Bool-sorted [t], true unless
    [top(t)] is [And]/[Or]/[Not], a result-Bool [Ite], or an [Eq] whose {e arguments} are
    Bool-sorted (a disguised iff — a connective the clausifier descends into, never an
    opaque EUF atom). Non-Bool terms are not atoms. *)
val is_atom : Term.t -> bool

(** [atom t] classifies an atom; requires [is_atom t]. *)
val atom : Term.t -> atom

(** [is_app t] holds for [App] nodes — the terms EUF congruence applies to. *)
val is_app : Term.t -> bool

(** [linear t] is the linear form when [t] is an [Arith] node, else [None]. *)
val linear : Term.t -> Term.linear option
