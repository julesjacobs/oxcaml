(** Exact parsing helpers for the SMT-LIB rational-literal grammar.

    The callbacks keep this module independent of any particular s-expression parser.
    A signed integer is either an unsigned decimal atom or unary minus applied to one.
    A fraction is [(/ p q)] with signed-integer [p] and [q], and [q <> 0].  No numeric
    value passes through a native integer or floating-point representation. *)

type fraction_error =
  | Not_a_fraction
  | Invalid_signed_integer
  | Zero_denominator

val decimal : string -> (Bigint.t * Bigint.t) option

val signed_integer
  :  atom:('a -> string option)
  -> minus:('a -> 'a option)
  -> 'a
  -> Bigint.t option

val fraction
  :  atom:('a -> string option)
  -> minus:('a -> 'a option)
  -> divide:('a -> ('a * 'a) option)
  -> 'a
  -> (Bigint.t * Bigint.t, fraction_error) result
