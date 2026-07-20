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
    disambiguate names before calling here.

    {b Reserved namespace (ADR-0012 R1 — the tier's one frozen touch).} The reserved
    [".oxsmt."] prefix ({!reserved_prefix}/{!is_reserved_name}) is this module's single
    source of truth (preprocessing, the session, and the parser reference it). The public
    declaration doors {!declare_fun} and {!declare_sort} REJECT any [".oxsmt.*"] name
    (both, since sort and function names share one namespace) — so a client holding a raw
    [Env] cannot forge a reserved symbol. Legitimate reserved symbols (preprocessing's
    fresh witnesses, the lemma tier's qvar placeholders) are minted through the capability
    door {!declare_reserved}, which requires the {!reserved_cap} that {!create_with_cap}
    hands out. That cap is {b per-env}: it authorizes reserved minting on {e its own} env
    only, so a cap obtained for one env is useless against another. *)

type t

(** An unforgeable, {b per-env} capability authorizing declaration of reserved
    [".oxsmt.*"] symbols on the env it was minted with (ADR-0012 R1 + per-env
    strengthening). No public constructor other than {!create_with_cap}; abstract, so a
    client holding only a [t] (e.g. via [Session.env]) cannot obtain one.
    {!declare_reserved} verifies the cap matches the target env, so even a cap from a
    different env is rejected. *)
type reserved_cap

(** The reserved fresh-symbol name prefix ([".oxsmt."]) and the predicate recognizing it.
    The single source of truth for the reservation guard shared by preprocessing, the
    session, and the parser. *)
val reserved_prefix : string

val is_reserved_name : string -> bool

(** Raised by {!declare_sort}/{!declare_fun} when asked to (re)declare a reserved name —
    the built-in [div]/[mod] (ADR-0003, protecting the pre-declared ranks) OR any
    [".oxsmt.*"] name (ADR-0012 R1, closing the forge door). *)
exception Reserved_symbol of string

(** [create ()] builds a fresh environment with [div]/[mod] pre-declared, discarding the
    reserved-minting capability. Use {!create_with_cap} when reserved symbols must be
    minted (a session does). *)
val create : unit -> t

(** [create_with_cap ()] is {!create} plus the env's {!reserved_cap}.
    {b Convention (ADR-0012):} solver-pipeline code calls this ONLY in [Session] — the
    session keeps the cap in private state and threads it to the legitimate
    reserved-symbol minters (preprocessing, the lemma tier); [Session.env] returns only
    the [t], never the cap. *)
val create_with_cap : unit -> t * reserved_cap

(** [declare_reserved cap t name rank] interns a reserved [".oxsmt.*"] [name] and records
    its rank, bypassing the public reserved-namespace rejection (ADR-0012 R1 capability
    mint). Raises [Invalid_argument] if [cap] was not minted with [t] (per-env), or if
    [name] is NOT a reserved name (this door is for reserved names only). *)
val declare_reserved : reserved_cap -> t -> string -> Rank.t -> Symbol.t

(** [declare_sort t name] interns [name] as a 0-arity uninterpreted sort symbol (v1:
    uninterpreted sorts are 0-arity). Raises {!Reserved_symbol} if [name] is a reserved
    name ([div]/[mod] or [".oxsmt.*"]). *)
val declare_sort : t -> string -> Symbol.t

(** [declare_fun t name rank] interns [name] and records its rank. Re-declaring a
    (non-reserved) name overwrites the rank. Raises {!Reserved_symbol} if [name] is a
    reserved name ([div]/[mod] or [".oxsmt.*"]). *)
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
