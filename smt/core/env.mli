(** Symbol environment (ADR-0003 Decision 6): maps function symbols to their {!Rank.t} and
    tracks declared uninterpreted sorts. One [Env.t] backs a session's {!Context.t}.
    [create] pre-declares the reserved [div]/[mod] built-in symbols (ADR-0003 Decision 5),
    reachable via {!div_sym}/{!mod_sym} for the div/mod-elimination pass.

    Symbol ids themselves are process-global (see {!Symbol}); an [Env] only owns the
    ranks.

    {b Shared name/symbol namespace (parser-layer obligation).} Because symbols are
    interned by name in one global table, sort names and function names share a namespace:
    [declare_sort t "S"] and [declare_fun t "S" r] return the {e same} symbol id. v1
    uninterpreted sorts are rare and 0-arity, so this is accepted; a front end (e.g. the
    SMT-LIB parser) that needs SMT-LIB's separate sort/function namespaces must
    disambiguate names before calling here. *)

type t

(** Raised by {!declare_sort}/{!declare_fun} when asked to (re)declare a reserved built-in
    name ([div] or [mod]); protects the pre-declared reserved ranks from being clobbered
    (R2). *)
exception Reserved_symbol of string

(** [create ()] builds a fresh environment with [div]/[mod] pre-declared. *)
val create : unit -> t

(** [declare_sort t name] interns [name] as a 0-arity uninterpreted sort symbol (v1:
    uninterpreted sorts are 0-arity). Raises {!Reserved_symbol} if [name] is [div]/[mod]. *)
val declare_sort : t -> string -> Symbol.t

(** [declare_fun t name rank] interns [name] and records its rank. Re-declaring a
    (non-reserved) name overwrites the rank. Raises {!Reserved_symbol} if [name] is
    [div]/[mod]. *)
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
