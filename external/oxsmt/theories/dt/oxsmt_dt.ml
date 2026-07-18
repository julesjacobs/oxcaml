(** Datatypes theory (GOALS Datatypes), built as an e-graph client over {!Oxsmt_euf.Euf}
    with no change to the SAT core. The datatype {e shape} registry it reads lives in the
    core ({!Oxsmt_core.Datatype_defs}); this library is the reasoning layer. *)

(** The DT theory: the four datatype axioms + constructor case splits, over an owned
    congruence-closure e-graph. Presented to the CDCL(T) seam as a standalone THEORY. *)
module Dt = Dt
