(** The structural registry for the array [select]/[store] operator symbols (arrays lane;
    the analogue of {!Datatype_defs} for the arrays theory).

    Array {e sorts} live in {!Sort.t} ([Sort.Array (index, element)]); array {e values}
    are ordinary {!Term.t} applications — [select]/[store] are [App (sym, args)] over
    symbols declared in {!Env} with a rank, so the term/rank machinery is unchanged.
    Because arrays are polymorphic, the front end mints a {e distinct} [select]/[store]
    symbol per (index, element) instantiation (a monomorphic rank each); what is NOT
    expressible in {!Env} is which of those symbols is a [select] vs a [store] and over
    which index/element sorts. This registry holds exactly that, keyed by symbol, and is
    the single source the arrays theory reads to classify an [App] head.

    Not frozen (deliberately): theory-facing plumbing, additive, outside the ADR-0003 core
    freeze. Built by the parser as array operators are used; read by the theory and the
    printer. Stdlib-only (I3). *)

type t

type role =
  | Select
  | Store

(** A registered array-operator symbol: its role and the (index, element) sorts of the
    array it operates over. A [Select] has rank [(Array(i,e), i) -> e]; a [Store] has rank
    [(Array(i,e), i, e) -> Array(i,e)]. *)
type entry =
  { role : role
  ; index : Sort.t
  ; element : Sort.t
  }

(** [op_symbol_name role ~index ~element] is the canonical, deterministic internal name of
    the [select]/[store] symbol for one array instantiation. Interning it (via the
    cap-gated reserved door {!Env.declare_reserved}, threaded as the parser's
    [?internal_mint] and reused directly by the arrays theory) yields the same {!Symbol.t}
    from any front end — the parser at parse time, and the arrays theory when it builds a
    fresh [select] for a read-over-write step — so those terms hash-cons to one identity
    and congruence composes.

    The name lives in the reserved [".oxsmt.arr."] namespace (board #58). This is what
    keeps it collision-proof, and NOT lexical illegality: the earlier ["@arr."] form was
    in fact a perfectly spellable SMT-LIB simple symbol ([@] and [.] are both in the
    simple-symbol charset), so a user [declare-fun] could alias it and hijack the theory's
    classification — a wrong verdict. Two enforcement layers now close that, in depth:
    - the [".oxsmt."] reserved prefix: the public {!Env.declare_fun}/{!Env.declare_sort}
      doors reject it, and only a capability holder mints it via {!Env.declare_reserved};
    - the [|] byte in the [role|index|element] sort-key separators: no SMT-LIB symbol
      form, simple or quoted, can carry a [|] (it closes a quoted symbol and is absent
      from the simple-symbol charset), so the name cannot even be written in parsed input,
      and the public [Env] doors reject the byte class outright. Distinct (role, index,
      element) triples yield distinct names. *)
val op_symbol_name : role -> index:Sort.t -> element:Sort.t -> string

(** [is_op_sym sym] is [true] when [sym]'s name is an array [select]/[store] op-symbol
    name (the [".oxsmt.arr."] prefix with a [|] sort-key separator, which excludes the
    ext-witness Skolem [.oxsmt.arr.ext.N]). The session's assert-side reserved-symbol gate
    uses this to admit a legitimate op symbol appearing as an [App] head in an ordinary
    assertion, while still rejecting every other reserved [.oxsmt.*] name (a coerced qvar
    or a captured preprocessing witness). A name-shape test is sound as that exemption
    because provenance is enforced at the minting door, not here: a [.oxsmt.arr.*] name
    can only acquire a rank — hence be applied via {!Context.app} — through the cap-gated
    {!Env.declare_reserved}, which only the parser's internal-mint hook and the arrays
    theory reach; a user [Symbol.intern] yields a rank-less symbol {!Context.app} refuses,
    and the public declare doors reject the prefix and the [|] byte. Registry-independent
    (it does not consult {!t}), so it is stable across the whole session. *)
val is_op_sym : Symbol.t -> bool

(** [is_op_name name] is [is_op_sym] on the raw name string — the op-symbol grammar
    without interning. This is the [admit] gate a [Session] gives its parse-time internal
    minter ({!Oxsmt_interface.Session.parse_minter}): the minter mints ONLY names matching
    this grammar, so the parser can intern array op symbols and nothing else (it can never
    forge the ext-witness [.oxsmt.arr.ext.N] — no [|] — a tester, a qvar, or a
    preprocessing witness). *)
val is_op_name : string -> bool

val empty : t
val is_empty : t -> bool

(** [add t sym role ~index ~element] registers [sym] as an array operator. Raises
    [Invalid_argument] if [sym] is already registered in a conflicting role (a front-end
    construction bug — a symbol is minted once per (role, index, element)). Re-adding the
    identical entry is idempotent. *)
val add : t -> Symbol.t -> role -> index:Sort.t -> element:Sort.t -> t

(** [role_of_sym t sym] is [Some entry] when [sym] is a registered [select]/[store]
    symbol; [None] otherwise. This is how the theory answers "is this [App] head an array
    operator, and over which sorts". *)
val role_of_sym : t -> Symbol.t -> entry option

(** [validate_ranks t ~rank_of] raises [Invalid_argument] if any registered operator's
    actual rank (via [rank_of], which the caller backs with the session {!Env}) disagrees
    with the canonical FULL SIGNATURE for its role and (index, element) sorts, or has no
    rank. Defence in depth against a caller registering a canonical [.oxsmt.arr.*] NAME
    minted at the wrong rank: [add] validates the name (which encodes role and sorts but
    not the rank itself) but not the rank, so the arrays theory could otherwise treat a
    wrong-arity OR wrong-sort uninterpreted function as an operator and, since the
    congruence engine is sort-agnostic, derive a wrong verdict. Full-signature (not just
    arity) is required: a right-arity/wrong-sort op passes an arity check but still
    corrupts ROW reasoning. Called at the registry-install door
    ({!Oxsmt_interface.Session.set_arrays}). *)
val validate_ranks : t -> rank_of:(Symbol.t -> Rank.t option) -> unit
