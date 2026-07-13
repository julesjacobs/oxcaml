(** SMT-LIB2 parser — TEST-ONLY. Reads the QF_UFLIA subset our printer emits (plus the
    constructs the public corpora need) into frozen-API {!Oxsmt_core.Term.t}s through a
    {!Oxsmt_core.Context.t}. It is the reverse of {!Oxsmt_smtlib.Printer} where the subset
    overlaps.

    {b This library is never linked into the shipped compiler} (DESIGN.md §3): only the
    printer ships. It exists to ingest benchmarks and round-trip our own dumps.

    [define-fun] macros are supported: parameters/result sort are read at definition time,
    the body is expanded by capture-avoiding substitution at each use site (macros, not
    recursive functions). Recursion — direct or mutual — and [define-fun-rec]/
    [define-funs-rec] are rejected as {!Unsupported}.

    Two failure modes, deliberately distinct (mirroring the gate reader):
    - {!Malformed}: input the reader cannot make sense of as a query — bad s-expr, unknown
      command shape, ill-sorted term, undeclared symbol, wrong arity, a [define-fun]
      application whose argument or body sort does not match its declaration.
    - {!Unsupported}: well-formed but outside our subset — a logic we do not model, a
      theory we do not implement, nonlinear multiplication, quantifiers, recursive
      [define-fun], or arithmetic exceeding native [int] range (the v1 core limitation). *)

exception Malformed of string
exception Unsupported of string

(** A universally-quantified assertion, [(assert (forall (binders) body))] (ADR-0012 lemma
    tier). The parser cannot construct the lemma itself — the bound variables must be
    minted as cap-gated placeholder qvars through {!Oxsmt_interface.Session} (mint-before-
    build, §1.3), which lives in a library this test-only parser must not depend on. So it
    records the binders and a deferred [build]: the driver mints one qvar per binder and
    passes their {!Oxsmt_core.Term.t} images (in binder order) to [build], which reads the
    body and any [:pattern] triggers with each binder bound to its qvar image. Nested
    [forall]s are flattened into one binder list; an [exists] is out of the fragment. *)
type lemma_src =
  { qvars : (string * Oxsmt_core.Sort.t) list (* forall binders, flattened, outer-first *)
  ; build : Oxsmt_core.Term.t array -> Oxsmt_core.Term.t * Oxsmt_core.Term.t list list
    (** [build qvar_images] is [(body, triggers)]; [qvar_images.(k)] substitutes for the
      k-th binder. May raise {!Malformed}/{!Unsupported} when the body is outside the
      subset — the driver maps that to a sound [unknown]. *)
  }

type t =
  { env : Oxsmt_core.Env.t
  ; ctx : Oxsmt_core.Context.t
  ; logic : string option
  ; status : Oxsmt_smtlib.Status.t option
  ; assertions : Oxsmt_core.Term.t list (* ground assertions, in file order *)
  ; datatypes : Oxsmt_core.Datatype_defs.t
    (* algebraic-datatype shapes from [declare-datatype(s)]: constructors, selectors,
         and testers, keyed by symbol, for the datatype theory. [empty] when none
         declared. *)
  ; arrays : Oxsmt_core.Array_defs.t
    (* the [select]/[store] operator symbols minted for the array instantiations the
         query uses, keyed by symbol, for the arrays theory. [empty] when the query uses
         no arrays. *)
  ; lemmas : lemma_src list (* the [(assert (forall ...))] assertions, in file order *)
  }

(** [parse src] parses a whole SMT-LIB2 document, creating a fresh {!Oxsmt_core.Env.t} and
    {!Oxsmt_core.Context.t}. *)
val parse : string -> t

(** [parse_into env ctx src] parses using a caller-supplied env and context, so the
    resulting terms share the tag stream (hash-cons identity) with terms already built in
    [ctx]. Used by the round-trip tests to compare via {!Oxsmt_core.Term.equal} within one
    context (the single-Context contract, ADR-0003). Re-declaring an already-known symbol
    is idempotent.

    [?internal_mint] (board #58 O-MINTER) is the opaque cap-backed minter for
    theory-internal reserved symbols ([.oxsmt.<theory>.*]) that must be minted mid-parse —
    arrays op symbols are per-(index sort, element sort) instantiations discovered only at
    the first [select]/[store] use, so they cannot be pre-minted at a declaration site. A
    [Session]-driven parse threads {!Oxsmt_interface.Session.parse_minter}, an opaque
    {!Oxsmt_core.Internal_minter.t} wrapping [Env.declare_reserved] over the session's
    private cap behind an [admit] gate: the parser mints a collision-proof sanctioned
    marker without ever holding the cap or a general closure (ADR-0012: only [Session]
    holds it). Omitting it (a standalone {!parse}, or a driver with no theory that mints
    at parse time) makes any mid-parse mint request raise {!Malformed} — never a silent
    success. *)
val parse_into
  :  ?internal_mint:Oxsmt_core.Internal_minter.t
  -> Oxsmt_core.Env.t
  -> Oxsmt_core.Context.t
  -> string
  -> t
