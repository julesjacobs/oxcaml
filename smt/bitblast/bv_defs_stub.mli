(** TEMPORARY local mirror of bv-front's forthcoming [Bitvec_defs] registry, so the
    circuit library and its oracle can be built and tested before the term/parser layer
    commits. On rebase onto task/bv-front this whole module is DELETED and replaced by a
    thin adapter that produces {!Blast.defs} from the real registry; nothing in
    {!Blast}/{!Bv_eval}/{!Bv_solve} changes.

    It keeps the frozen core untouched: a bit-vector sort is an [Sort.uninterpreted] whose
    width this registry records; a bit-vector term is an ordinary [App] whose symbol this
    registry classifies into a {!Bv_op.t}. That is exactly the Datatype_defs pattern and
    exactly the contract proposed to bv-front. *)

open Oxsmt_core

type t

(** A registry over [env]; all symbols it mints are declared in [env], so terms built
    through the matching {!Context} sort-check. *)
val create : Env.t -> t

(** The read-side view the circuit library consumes. *)
val defs : t -> Blast.defs

(** [sort t w] is the width-[w] bit-vector sort (interned once per width). *)
val sort : t -> int -> Sort.t

(** [var t ctx name w] is a fresh width-[w] bit-vector variable named [name]. *)
val var : t -> Context.t -> string -> int -> Term.t

(** [const t ctx v w] is the width-[w] constant with value [v] (reduced mod 2^w). *)
val const : t -> Context.t -> Bigint.t -> int -> Term.t

(** Build an [App] of a bit-vector operator, inferring result width where the operator
    fixes it. [result_width] is required for the width-changing operators
    (extract/extends/concat) and ignored otherwise. *)
val op : t -> Context.t -> ?result_width:int -> Bv_op.t -> Term.t list -> Term.t
