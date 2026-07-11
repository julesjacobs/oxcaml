open Oxsmt_core

(** An independent, fresh-from-grammar SMT-LIB2 reader for the QF_UFLIA subset (N-version
    redundancy, task #74 — deliberately NOT the shipped [oxsmt_smtlib] parser nor the
    gate's reader; it shares no code with them). It builds frozen-API {!Term.t}s through
    {!Context}'s smart constructors, so every term it returns is well-sorted and
    hash-consed by construction, and records enough declaration info for {!Model} to type
    the sidecar tokens.

    Reject-don't-guess: any construct outside the subset raises {!Unsupported}; any
    ill-formed / ill-sorted / undeclared input raises {!Malformed}. Nothing is silently
    reinterpreted.

    Subset. Commands: [set-logic] (QF_UF / QF_LIA / QF_UFLIA / QF_IDL / QF_RDL),
    [set-info :status], [declare-sort] (arity 0), [declare-fun], [declare-const],
    [assert], [check-sat], [exit]; [set-option] and non-[:status] [set-info] are ignored.
    Terms: [true]/[false], numerals, [and]/[or]/[not]/[=>], [ite], [=]/[distinct],
    chainable [<=]/[<]/[>=]/[>], [+]/[-]/[*] (linear only), [div]/[mod]/[abs], parallel
    [let], [(! t ...)] annotations (attributes dropped), [|quoted symbols|], and declared
    symbols. [define-fun], quantifiers, [push]/[pop], and compound sorts are
    {!Unsupported}. *)

type status =
  | Sat
  | Unsat
  | Unknown
  | No_status

(** The declaration table, consumed by {!Model} to interpret sidecar tokens against each
    symbol's declared sort. *)
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
