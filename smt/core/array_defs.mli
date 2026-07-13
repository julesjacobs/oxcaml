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
    the [select]/[store] symbol for one array instantiation. Interning it (via
    {!Env.declare_fun}) yields the same {!Symbol.t} from any front end — the parser at
    parse time, and the arrays theory when it builds a fresh [select] for a
    read-over-write step — so those terms hash-cons to one identity and congruence
    composes. The name is not a legal simple SMT-LIB symbol (the ["@arr."] prefix), so it
    cannot collide with a user declaration; and it is not the reserved [".oxsmt."] prefix,
    so [Env.declare_fun] admits it. Distinct (role, index, element) triples yield distinct
    names. *)
val op_symbol_name : role -> index:Sort.t -> element:Sort.t -> string

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
