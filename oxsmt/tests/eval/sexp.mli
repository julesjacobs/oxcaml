(** Minimal s-expression reader (N-version: written fresh from the SMT-LIB lexical
    grammar, shared by the {!Reader} .smt2 front end and the {!Eval_model} sidecar reader).
    Deliberately tiny and independent of every other reader in the tree.

    Lexical rules covered (the QF_UFLIA subset + the model sidecar): parentheses,
    whitespace separation, [;]-to-end-of-line comments, and [|quoted symbols|]. A quoted
    symbol is kept distinct from an ordinary atom so a symbol literally named [and] or
    [Int] can never be read as the operator/sort of the same spelling. Strings, [#]-radix
    numerals, and [:] keywords-with-values beyond bare [:kw] tokens are out of scope
    (rejected loudly). *)

type t =
  | Atom of string (* an unquoted token: operator, keyword, numeral, or plain symbol *)
  | Quoted of string (* a |...| symbol; always a symbol, never an operator/sort/keyword *)
  | List of t list

(** Raised on any lexical error (unbalanced parens, unterminated [|...|], stray [)]). *)
exception Parse_error of string

(** [parse_all s] returns every top-level s-expression in [s], in order. *)
val parse_all : string -> t list
