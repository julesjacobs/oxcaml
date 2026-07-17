open Oxsmt_core

(** An independent, fresh-from-grammar SMT-LIB2 reader for the QF_UFLIA and QF_BV
    subsets (N-version redundancy, task #74 — deliberately NOT the shipped
    [oxsmt_smtlib] parser nor the
    gate's reader; it shares no code with them). It builds frozen-API {!Term.t}s through
    {!Context}'s smart constructors, so every term it returns is well-sorted and
    hash-consed by construction, and records enough declaration info for {!Eval_model} to
    type the sidecar tokens.

    Reject-don't-guess: any construct outside the subset raises {!Unsupported}; any
    ill-formed / ill-sorted / undeclared input raises {!Malformed}. Nothing is silently
    reinterpreted.

    Subset. Commands: [set-logic] (QF_UF / QF_LIA / QF_UFLIA / QF_IDL / QF_RDL /
    QF_BV / QF_UFBV),
    [set-info :status], [declare-sort] (arity 0), [declare-fun], [declare-const],
    [assert], [check-sat], [exit]; [set-option] and non-[:status] [set-info] are ignored.
    [define-fun] is supported as a non-recursive macro (SMT-LIB 2.6 §4.2.2): its body is
    stored unexpanded and substituted at each use site (zero parameters = a named
    constant). Recursion is rejected — a macro referring to itself is {!Unsupported}, as
    are [define-fun-rec] / [define-funs-rec]. Body validation is LAZY (a documented
    deviation from §4.2.2, which elaborates the body at definition time): the body is
    sort-checked and its symbols resolved only when the macro is USED, so an UNUSED macro
    with an ill-formed body, or a body forward-referencing a symbol declared after the
    definition, is accepted. Both are fail-closed on use (an unresolved symbol / sort
    mismatch raises {!Malformed}) and arise only on ill-formed input, so a well-formed
    model is never mis-satisfied.

    Terms: [true]/[false], numerals, [and]/[or]/[not]/[=>], [ite], [=]/[distinct],
    chainable [<=]/[<]/[>=]/[>], [+]/[-]/[*] (linear only), [div]/[mod]/[abs], parallel
    [let], [(! t ...)] annotations (attributes dropped), [|quoted symbols|], declared
    symbols, and [define-fun] applications. The fixed-width subset adds [(_ BitVec w)],
    [#b]/[#x]/[(_ bvN w)] literals, bitwise and modular arithmetic, total shifts and
    division/remainder, signed and unsigned comparisons, concat/extract/extensions,
    rotations, and repeat. These terms use an evaluator-private symbol vocabulary and do
    not consult the solver's parser, bitvector classifier, or evaluator. Quantifiers,
    [push]/[pop], and compound sorts other than [BitVec] are {!Unsupported}. *)

type status =
  | Sat
  | Unsat
  | Unknown
  | No_status

(** The declaration table, consumed by {!Eval_model} to interpret sidecar tokens against
    each symbol's declared sort. *)
module Decls : sig
  type t

  (** The sort of a nullary symbol (a [declare-const] or arity-0 [declare-fun]). *)
  val const_sort : t -> string -> Sort.t option

  (** The rank of an arity-≥1 function symbol. *)
  val fun_rank : t -> string -> Rank.t option

  (** Resolve a sort name: a declared uninterpreted sort, or [Int]/[Bool]. *)
  val sort_by_name : t -> string -> Sort.t option

  val fold_consts : (string -> Sort.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_funs : (string -> Rank.t -> 'a -> 'a) -> t -> 'a -> 'a
end

type query =
  { assertions : Term.t list (* in file order; the model must satisfy every one *)
  ; status : status
  ; decls : Decls.t
  ; context : Context.t
  }

(** Out-of-subset construct (a well-formed thing we do not handle). *)
exception Unsupported of string

(** Ill-formed / ill-sorted / undeclared input. *)
exception Malformed of string

val read_string : string -> query
val read_file : string -> query
