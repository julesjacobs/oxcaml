(** Arrays theory (QF_AX: functional, extensional arrays), built as an e-graph client over
    {!Oxsmt_euf.Euf} with no change to the SAT core. The select/store symbol
    classification it reads lives in the core ({!Oxsmt_core.Array_defs}); this library is
    the reasoning layer (read-over-write + extensionality). *)

(** The arrays theory: ROW propagation + a lazy index split + extensionality witnesses,
    over an owned congruence-closure e-graph. Presented to the CDCL(T) seam as a
    standalone THEORY. *)
module Arr = Arr
