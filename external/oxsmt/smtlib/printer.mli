(** SMT-LIB2 printer (SHIPPED code). Renders a session — an {!Env.t}, an ordered list of
    assertion {!Term.t}s, and an optional expected {!Status.t} — as a complete SMT-LIB2
    script. Integer sessions retain the existing [QF_UFLIA] output. With [OXSMT_LRA]
    enabled, Real sessions select [QF_LRA] or [QF_UFLRA] from their term contents.

    Stdlib-only (INVARIANTS.md I3): depends on {!Oxsmt_core}, nothing else. This is the
    stable interchange format for the Lean oracle and the public benchmark corpora
    (DESIGN.md §3). Output is deterministic: the same session prints byte-identical text
    every run (INVARIANTS.md I6).

    {b Rendering choices} (documented so standard tools and our own parser agree):
    - Integer constants: [n >= 0] as the numeral; [n < 0] as [(- N)] with [N] the absolute
      value (so [min_int] is handled without a negation overflow).
    - Exact Real constants: an integral value is a decimal such as [3.0]; a non-integral
      value is [(/ p q)] with positive [q], and a negative value uses outer unary minus.
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

(** Raised when a symbol name cannot be faithfully rendered as an SMT-LIB symbol. Because
    quoting is purely lexical ([|s|] and [s] denote the {e same} symbol), three classes are
    unrepresentable and are refused rather than mis-rendered:
    - names containing [|] or [\\] ([|...|] has no escape);
    - names equal to a predefined function/operator symbol ([+ - * abs <= < >= > = distinct
      => and or not xor ite true false], plus [/] when LRA is enabled) — [|+|] is still
      the operator [+]; and, in sort position, names equal to a predefined sort
      ([Int]/[Bool], plus [Real] when LRA is enabled);
    - the empty name.
    A name that is merely a reserved {e word} ([let], [as], [forall], [_], [!], …) is
    representable and is emitted [|quoted|], not refused. *)
exception Unsupported of string

(** [quote_symbol name] renders a function/constant symbol name: returns [name] unchanged
    when it is a simple SMT-LIB symbol (nonempty, every char in
    [\[a-zA-Z0-9~!@$%^&*_+=<>.?/-\]], not starting with a digit) and not a reserved word,
    otherwise [|name|]. Raises {!Unsupported} for the unrepresentable classes above (a
    predefined operator name, [|]/[\\], or empty). [div]/[mod] are {e not} refused: they
    are the reserved built-ins and print bare as operator applications. *)
val quote_symbol : string -> string

(** [print_term ?datatypes t] renders a single term as an SMT-LIB2 s-expression (no
    trailing newline). Exposed for tests and reuse. [?datatypes] MUST be the same registry
    {!print_session} is given when [t] may contain a tester application: a tester renders
    as [((_ is C) t)] only when the registry resolves its symbol, so omitting the registry
    (the default empty one) on a tester term would render it under its internal symbol
    name — a DIFFERENT byte string than {!print_session} produces. With the same registry
    the two entry points render byte-identically. *)
val print_term
  :  ?datatypes:Oxsmt_core.Datatype_defs.t
  -> ?arrays:Oxsmt_core.Array_defs.t
  -> Oxsmt_core.Term.t
  -> string

(** [print_session ?status env assertions] renders a complete SMT-LIB2 script:

    {[
      (set-info :status STATUS)   ; only when [status] is given
      (set-logic QF_UFLIA)        ; or QF_LRA/QF_UFLRA for Real content
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
    name.

    [?datatypes] supplies the algebraic-datatype shapes (from the parser / session). When
    a session uses datatype sorts it emits, after the uninterpreted sorts and before the
    function declarations, a single [(declare-datatypes ((T 0)...) (ctor-list...))] block
    (all datatypes together, mutual recursion included); the constructor / selector /
    tester [declare-fun]s are suppressed (implicit in the block), testers render as
    [((_ is C) t)], and the logic becomes [QF_UFDT]. Omitting it (the default empty
    registry) reproduces the byte-identical QF_UFLIA behaviour for datatype-free sessions.
    Raises {!Unsupported} if a datatype sort is used with no matching registry entry.

    [?arrays] supplies the array [select]/[store] symbol registry (from the parser /
    session). When non-empty, those symbols render as [(select ...)] / [(store ...)]
    rather than their internal per-instantiation names, are suppressed from the
    [declare-fun] block (they are theory builtins), array sorts render as [(Array I E)],
    and the logic becomes [QF_AUFLIA]. Omitting it (the default empty registry) is
    byte-identical for array-free sessions. *)
val print_session
  :  ?status:Status.t
  -> ?datatypes:Oxsmt_core.Datatype_defs.t
  -> ?arrays:Oxsmt_core.Array_defs.t
  -> Oxsmt_core.Env.t
  -> Oxsmt_core.Term.t list
  -> string
