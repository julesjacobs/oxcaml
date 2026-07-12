(* Gate outcomes (DESIGN.md §8). Certified/Refuted/Inconclusive/Encode_error are the
   certification outcomes; Malformed/Unsupported come from the reader before encoding;
   No_status means the file carries no claim to certify.

   REFUTED is the only ship-stopping outcome and is always a KERNEL-checked proof of the
   opposite claim: the refutation witness check uses [decide] only — never the
   compiler-trusted [native_decide] — so a compiler / [Lean.ofReduceBool] miscompile can
   never manufacture a false ship-stopper (ADR-0006 Decision 3, review AP4; see NOTES.md).
   CERTIFIED, by contrast, is not kernel-only: a sat witness may certify via
   native_decide, and that (weaker) tier is recorded — see [Certified] below. *)

type t =
  | Certified of string
    (* the tactic that discharged the goal — the trust-tier provenance (ADR-0006 Decision
       3): [native_decide] is compiler-trusted (Lean.ofReduceBool axiom + compiler),
       everything else ([grind]/[decide]/[omega]) is kernel-checked. Carried so STATUS
       (status_gen parse_gate_log) can report the kernel-vs-compiler tier mix honestly. *)
  | Refuted of string (* how it was refuted *)
  | Inconclusive of string
  | Encode_error of string
  | Malformed of string
  | Unsupported of string
  | No_status

let tag = function
  | Certified _ -> "CERTIFIED"
  | Refuted _ -> "REFUTED"
  | Inconclusive _ -> "INCONCLUSIVE"
  | Encode_error _ -> "ENCODE_ERROR"
  | Malformed _ -> "MALFORMED"
  | Unsupported _ -> "UNSUPPORTED"
  | No_status -> "NO_STATUS"
;;

let detail = function
  | No_status -> ""
  | Certified s
  | Refuted s
  | Inconclusive s
  | Encode_error s
  | Malformed s
  | Unsupported s -> s
;;

(* A ship-stopping outcome blocks the gate (DESIGN.md §8 verdict asymmetry). *)
let is_ship_stopper = function
  | Refuted _ -> true
  | _ -> false
;;

let is_certified = function
  | Certified _ -> true
  | _ -> false
;;

(* Exit code for the single-file [certify] subcommand. Malformed vs Unsupported get
   distinct codes (DESIGN task: "exit code distinguishes unsupported from malformed"). *)
let exit_code = function
  | Certified _ -> 0
  | Refuted _ | Encode_error _ -> 1 (* ship-stopper / encoder bug *)
  | Inconclusive _ -> 2
  | Malformed _ -> 3
  | Unsupported _ -> 4
  | No_status -> 5
;;
