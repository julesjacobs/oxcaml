(** SMT-LIB2 printer (SHIPPED code). Renders a session — an {!Env.t}, an ordered list of
    assertion {!Term.t}s, and an optional expected {!Status.t} — as a complete SMT-LIB2
    script over the [QF_UFLIA] logic.

    Stdlib-only (INVARIANTS.md I3): depends on {!Oxsmt_core}, nothing else. This is the
    stable interchange format for the Lean oracle and the public benchmark corpora
    (DESIGN.md §3). Output is deterministic: the same session prints byte-identical text
    every run (INVARIANTS.md I6).

    {b Rendering choices} (documented so standard tools and our own parser agree):
    - Integer constants: [n >= 0] as the numeral; [n < 0] as [(- N)] with [N] the absolute
      value (so [min_int] is handled without a negation overflow).
    - [Arith] linear forms render as a sum whose summands are [(<STAR> c t)] products
      (with [<STAR>] the multiplication operator): a coefficient of 1 renders the term
      bare, a nonzero constant is the final summand (omitted when 0), and a lone product
      summand is emitted without a wrapping [+] (never a unary [+]). Negative
      coefficients/constants use the [(- N)] form.
    - Order atoms ([Le]) render as [(<= arg 0)].
    - [Eq], [Not], [And], [Or], [Ite] render directly ([=]/[not]/[and]/[or]/[ite]). A
      Bool-sorted [Eq] renders as [(= a b)] (an iff); note the current gate reader rejects
      Bool-sorted [=] (the tracked M0-gate-iff gap), so such dumps are not gate-encodable
      yet — that is not this printer's bug.
    - The reserved [div]/[mod] symbols render as [(div a b)]/[(mod a b)] (their symbol
      name is [div]/[mod]) and are never emitted as declarations.
    - [distinct]/[abs] never appear: they desugar at construction to [Not]/[Eq] and [Ite],
      which is what gets printed. *)

(** Raised when a symbol name cannot be represented as an SMT-LIB symbol — it contains a
    [|] or [\\], which [|...|] quoting cannot escape. We refuse rather than emit
    unparseable text. *)
exception Unsupported of string

(** [quote_symbol name] returns [name] unchanged when it is a simple SMT-LIB symbol
    (nonempty, every char in [\[a-zA-Z0-9~!@$%^&*_+=<>.?/-\]], not starting with a digit),
    otherwise [|name|]. Raises {!Unsupported} if [name] contains [|] or [\\]. *)
val quote_symbol : string -> string

(** [print_term t] renders a single term as an SMT-LIB2 s-expression (no trailing
    newline). Exposed for tests and reuse. *)
val print_term : Oxsmt_core.Term.t -> string

(** [print_session ?status env assertions] renders a complete SMT-LIB2 script:

    {[
      (set-info :status STATUS)   ; only when [status] is given
      (set-logic QF_UFLIA)
      (declare-sort S 0)          ; uninterpreted sorts, first-use order
      (declare-fun f (Int) Int)   ; arity >= 1 symbols, first-use order
      (declare-const x Int)       ; arity 0 symbols
      (assert ...)                ; one per assertion, in the given order
      (check-sat)
    ]}

    Declarations are collected by a deterministic left-to-right, depth-first walk of
    [assertions] (looking each symbol's rank up in [env]); all uninterpreted sorts are
    emitted before all function/constant declarations, so every declaration precedes its
    uses. A symbol declared but never used in an assertion is not emitted (it cannot
    affect the verdict). Raises {!Unsupported} via {!quote_symbol} on an unrepresentable
    name. *)
val print_session
  :  ?status:Status.t
  -> Oxsmt_core.Env.t
  -> Oxsmt_core.Term.t list
  -> string
