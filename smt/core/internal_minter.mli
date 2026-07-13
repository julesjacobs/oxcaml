(** Board #58 O-MINTER: an opaque, cap-backed minter for theory-internal reserved
    ([".oxsmt.*"]) symbols that a front end must mint mid-parse — arrays op symbols are
    per-(index sort, element sort) instantiations discovered only at the first
    [select]/[store] use, so they cannot be pre-minted at a declaration site.

    It wraps {!Env.declare_reserved} closed over an env's private {!Env.reserved_cap}: the
    holder can mint a collision-proof internal symbol WITHOUT ever obtaining the cap
    itself (ADR-0012 keeps the cap with whoever minted the env). [t] is abstract and
    carries no projection back to the cap or the raw closure — so handing a [t] to a front
    end grants exactly a NARROWED minting effect and nothing re-delegatable. This replaces
    the earlier design that exposed [Env.declare_reserved cap env] as a bare, {e general}
    [string -> Rank.t -> Symbol.t] closure on the session surface (any holder could then
    mint an arbitrary reserved name — the O-MINTER finding).

    {b The [admit] gate is the narrowing.} Every {!mint} is refused unless [admit name]
    holds, so a [t] can only mint the marker names its creator explicitly sanctioned. A
    [Session]-issued minter admits {e only} the parse-time theory vocabulary (arrays op
    symbols, bit-vector markers) and so can never forge the sensitive reserved namespaces
    (the extensionality witness [.oxsmt.arr.ext.*], datatype testers, qvars, preprocessing
    witnesses) — those are minted directly through {!Env.declare_reserved} by trusted
    session/theory code, never through this front-end door. On trunk no theory mints at
    parse time, so the session's minter admits nothing. *)

type t

(** [create ~admit cap env] builds the minter for [env]. [cap] must be the
    {!Env.reserved_cap} minted with [env] (re-checked by {!Env.declare_reserved} on every
    {!mint}, so a cap for a different env yields a minter that always raises). [admit] is
    the sanctioned-name gate: {!mint} refuses any [name] for which [admit name] is false.
    Only a cap holder can call this. *)
val create : admit:(string -> bool) -> Env.reserved_cap -> Env.t -> t

(** [mint t name rank] interns the reserved [name] with [rank] through the cap door.
    Raises [Invalid_argument] if [admit name] is false (the name is not a sanctioned
    marker), or per {!Env.declare_reserved} if [name] is not a [".oxsmt.*"] reserved name
    or the wrapped cap does not match its env. *)
val mint : t -> string -> Rank.t -> Symbol.t
