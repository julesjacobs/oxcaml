(* Router for the DT + LIA combination (task #47, bugreport 03). Ownership and the
   ℤ-trichotomy split are IDENTICAL to {!Uflia_router}: a datatype (dis)equality already
   routes to the congruence child [A] (a [Theory_view.Equality] over a datatype sort -> A,
   like an uninterpreted-sort equality), a LIA order atom [Le_zero] to the arithmetic
   child [B], and an Int equality to [Both] — the same structural dispatch EUF+LIA uses.
   So the congruence child (here the DT theory) and LIA share every Int-sorted boundary
   term, and a DT-derived Int equality (e.g. selector evaluation [key (Node _ k _) = k])
   reaches LIA through the classic disagreement/trichotomy path exactly as [f x = x + y]
   does in QF_UFLIA.

   What differs is the two combination flags: the congruence child IS the DT theory
   ([congruence_models_datatypes] — so datatype-sorted terms are not degraded at Sat
   certification; the child's axiom-validating Final has already certified them), and the
   in-search DT+LIA fabric drive is gated behind the dedicated runtime lever
   [OXSMT_COMBINE_INSEARCH] (see [fabric_disabled] below) — default OFF is byte-identical
   to the classic no-fabric path. *)
include Uflia_router

(* Dedicated runtime lever OXSMT_COMBINE_INSEARCH (read once at module init). UNSET (or
   any value other than 1/true/yes) => [fabric_disabled = true] => the classic no-fabric
   path, byte-identical to trunk. SET => [fabric_disabled = false] => [Combine] activates
   the fabric drive + create-time merge-consumer setup for this DT+LIA instantiation
   (Stage C mechanism I: in-search congruence propagation across the DT/LIA seam).
   Independent of the global [OXSMT_NO_FABRIC] toggle. *)
let fabric_disabled =
  not
    (match Sys.getenv_opt "OXSMT_COMBINE_INSEARCH" with
     | Some ("1" | "true" | "yes") -> true
     | _ -> false)
;;

let congruence_models_datatypes = true
