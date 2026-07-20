(** The signature of an uninterpreted function symbol (ADR-0003 Decision 6): argument
    sorts and result sort. A predicate is a symbol whose [codomain] is [Sort.bool]; a
    nullary constant or program variable has an empty [domain]. Ranks live in {!Env},
    keyed by symbol. *)

type t =
  { domain : Sort.t Iarr.t
  ; codomain : Sort.t
  }

val create : Sort.t list -> Sort.t -> t
val arity : t -> int
