(** The caller supplies a nonempty, duplicate-free enumeration of a finite
    lattice. [equal], [le], [join], and [meet] must agree on that carrier. *)
module type Lattice = sig
  type elt

  val elements : elt list

  val equal : elt -> elt -> bool

  val le : elt -> elt -> bool

  val join : elt -> elt -> elt

  val meet : elt -> elt -> elt
end

(** An immutable exact relation over one lattice. Variable indices use de Bruijn
    order, with index zero denoting the newest variable. Operations that exceed
    the finite search or formula-size budget return [Limit]. *)
module Make (L : Lattice) : sig
  type elt = L.elt

  type morph

  type term =
    | Const of elt
    | Var of int
    | Join of term * term
    | Meet of term * term
    | Apply of morph * term

  type qf =
    | True
    | Le of term * term
    | And of qf * qf
    | Or of qf * qf
    | Not of qf

  type formula =
    | Plain of qf
    | Conj of formula * formula
    | Disj of formula * formula
    | Neg of formula
    | Exists of formula
    | Forall of formula

  type state

  type error =
    | Invalid_scope
    | Limit

  val empty : int -> (state, error) result

  val morph : (elt -> elt) -> morph

  val assert_clause : state -> qf -> (state, error) result

  val fresh : state -> (state, error) result

  val merge : state -> state -> (state, error) result

  val assert_quantified : state -> formula -> (state, error) result

  val project_first : state -> (state, error) result

  val project : state -> int -> (state, error) result

  val copy_first : state -> (state, error) result

  val copy_index : state -> int -> (state, error) result

  val holds : state -> elt list -> (bool, error) result

  val bounds : ?limit:int -> state -> int -> ((elt * elt) option, error) result

  val query : ?limit:int -> state -> qf -> (bool, error) result

  val query_quantified : ?limit:int -> state -> formula -> (bool, error) result
end
