(* TEST readonly_files = "sc_diamond_base.mli"; setup-ocamlc.byte-build-env; module =
   "sc_diamond_base.mli"; ocamlc.byte; module = "sc_diamond.ml"; ocamlc.byte;
*)

(* SOUNDNESS CAMPAIGN — extension B (cross-.cmi diamond persistence).

   Complements the in-memory family-4 graphs with REAL separate compilation.
   [sc_diamond_base.mli] is compiled to a .cmi; this unit imports it and re-exports it via
   TWO independent paths (Ra, Rb), then forces the refined types produced by the two
   round-trips to MEET:

   - [pos] (a refined type alias) via both paths must be the SAME type;
   - the sibling-reference value [g : int{ _ = base }] read via both paths must have EQUAL
     type (predicate reference-heads must be rewritten consistently across each
     independent .cmi import — the exact machinery whose stamp-mismatch bug was fixed by
     the includemod value-pairing);
   - the functor [Make]'s result predicate [_ = cap] (naming its own parameter) must
     survive instantiation.

   Everything here MUST COMPILE. A "values do not match" / predicate clash would be a
   persistence-corruption regression (over-strict); silent acceptance of a type mismatch
   would be the opposite failure. Both are findings. *)

module type Base = module type of Sc_diamond_base

module Ra : Base = Sc_diamond_base
module Rb : Base = Sc_diamond_base

(* pos through two independent cmi round-trips is one type. *)
let diamond_pos (x : Ra.pos) : Rb.pos = x

(* the sibling-reference refined value read via both paths must unify (list element
   unification forces rigid predicate + reference-head equality). *)
let diamond_g = [ Ra.g; Rb.g ]

(* functor result predicate survives instantiation through the imported cmi. *)
module M1 = Ra.Make (struct
    let cap = 7
  end)

module M2 = Rb.Make (struct
    let cap = 7
  end)

let diamond_v = [ M1.v; M2.v ]
