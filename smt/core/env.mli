(** Symbol environment (ADR-0003 Decision 6): maps function symbols to their {!Rank.t} and
    tracks declared uninterpreted sorts. One [Env.t] backs a session's {!Context.t}.
    [create] pre-declares the reserved [div]/[mod] built-in symbols (ADR-0003 Decision 5),
    reachable via {!div_sym}/{!mod_sym} for the div/mod-elimination pass.

    Symbol ids themselves are process-global (see {!Symbol}); an [Env] only owns the
    ranks. *)

type t

(** [create ()] builds a fresh environment with [div]/[mod] pre-declared. *)
val create : unit -> t

(** [declare_sort t name] interns [name] as a 0-arity uninterpreted sort symbol (v1:
    uninterpreted sorts are 0-arity). *)
val declare_sort : t -> string -> Symbol.t

(** [declare_fun t name rank] interns [name] and records its rank. Re-declaring the same
    name overwrites the rank. *)
val declare_fun : t -> string -> Rank.t -> Symbol.t

(** [rank t sym] is the recorded rank of [sym]. Raises [Not_found] if [sym] has no rank in
    [t] (e.g. an undeclared symbol or a sort symbol). {!Context.app} turns this into a
    [Term.Sort_error]. *)
val rank : t -> Symbol.t -> Rank.t

(** The reserved [div]/[mod] symbols, ranks [(Int, Int) -> Int]. Deviation from the ADR's
    four-function [Env] sketch: exposed so {!Context} can build [div]/[mod] applications
    without re-interning by name. *)
val div_sym : t -> Symbol.t

val mod_sym : t -> Symbol.t
