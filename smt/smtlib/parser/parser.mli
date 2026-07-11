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

type t =
  { env : Oxsmt_core.Env.t
  ; ctx : Oxsmt_core.Context.t
  ; logic : string option
  ; status : Oxsmt_smtlib.Status.t option
  ; assertions : Oxsmt_core.Term.t list (* in file order *)
  }

(** [parse src] parses a whole SMT-LIB2 document, creating a fresh {!Oxsmt_core.Env.t} and
    {!Oxsmt_core.Context.t}. *)
val parse : string -> t

(** [parse_into env ctx src] parses using a caller-supplied env and context, so the
    resulting terms share the tag stream (hash-cons identity) with terms already built in
    [ctx]. Used by the round-trip tests to compare via {!Oxsmt_core.Term.equal} within one
    context (the single-Context contract, ADR-0003). Re-declaring an already-known symbol
    is idempotent. *)
val parse_into : Oxsmt_core.Env.t -> Oxsmt_core.Context.t -> string -> t
