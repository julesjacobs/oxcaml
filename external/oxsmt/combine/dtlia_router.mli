(** The concrete {!Combine.ROUTER} for the DT + LIA combination (task #47, bugreport 03):
    congruence child [A] = the datatype theory (an EUF-based e-graph client), arithmetic
    child [B] = LIA. Ownership and the ℤ-trichotomy equality split are identical to
    {!Uflia_router} (datatype/uninterpreted (dis)equality → [A], order atom → [B], Int
    equality → [Both]); it differs only in the two combination flags — forcing the classic
    no-fabric path ([fabric_disabled = true]) and letting the DT congruence child own the
    datatype model ([congruence_models_datatypes = true]). *)

include Combine.ROUTER
