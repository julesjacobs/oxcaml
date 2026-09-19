(** These return contracts describe completed runs; the entry points are partial. *)
open Copy_spec
open Generalize_spec
open Hm_effective_execution_spec
module H := Pref.Heap
module D := Hm_declarative
module T := Fast_term
module C := Hm_routed_context
module B := Borrow_iarray.Owned_array

type inference = #{value : node Pref.t option @@ aliased; state : node Pref.token;
  pool : pool @@ ghost; execution : execution @@ ghost;
  physical : pool @@ aliased; pools : pool B.t; routing : C.context @@ ghost}

type answer = #{value : node Pref.t option @@ aliased; state : node Pref.token;
  pool : pool @@ ghost; execution : execution @@ ghost}

val closed_compiled :
  (input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)}) @ immutable ->
    {r : inference |
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
        r.#execution (Pref.own r.#state) r.#pool
      && source r.#execution === T.source input && r.#value === result r.#execution} @
        unique

val closed_hm :
  (e : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
    {r : answer |
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
        r.#execution (Pref.own r.#state) r.#pool
      && source r.#execution === e && r.#value === result r.#execution} @ unique
