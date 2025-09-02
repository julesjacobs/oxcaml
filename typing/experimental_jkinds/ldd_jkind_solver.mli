module Make
    (Lat : Lattice_intf.LATTICE)
    (Ty : sig
      type t

      val compare : t -> t -> int
      val to_string : t -> string
    end)
    (Constr : sig
      type t

      val compare : t -> t -> int
      val to_string : t -> string
    end) : sig
  type ty = Ty.t
  type constr = Constr.t
  type lat = Lat.t
  type atom = { constr : constr; arg_index : int }

  module RigidName : sig
    type t = Atom of atom | Ty of ty

    val compare : t -> t -> int
    val to_string : t -> string

    val atomic : constr -> int -> t
  end

  type poly
  type kind

  type ops = {
    const : lat -> kind;
    join : kind list -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
    rigid : ty -> kind;
  }

  type ckind = ops -> kind

  type constr_decl =
    | Ty of { args : ty list; kind : ckind; abstract : bool }
    | Poly of poly * poly list

  type env = { kind_of : ty -> ckind; lookup : constr -> constr_decl }
  type solver = { ops : ops; constr_kind_poly : constr -> poly * poly list }

  val make_solver : env -> solver
  val constr_kind_poly : solver -> constr -> poly * poly list
  val leq : solver -> ckind -> ckind -> bool
  val round_up : solver -> ckind -> lat
  val clear_memos : unit -> unit
  val pp : poly -> string
  val pp_debug : poly -> string
end

