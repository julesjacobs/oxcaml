(* Gate outcomes (DESIGN.md §8). Certified/Refuted/Inconclusive/Encode_error are the
   certification outcomes; Malformed/Unsupported come from the reader before encoding;
   No_status means the file carries no claim to certify.

   REFUTED is the only ship-stopping outcome and is always a kernel-checked proof of the
   opposite claim (see NOTES.md). *)

type t =
  | Certified
  | Refuted of string (* how it was refuted *)
  | Inconclusive of string
  | Encode_error of string
  | Malformed of string
  | Unsupported of string
  | No_status

let tag = function
  | Certified -> "CERTIFIED"
  | Refuted _ -> "REFUTED"
  | Inconclusive _ -> "INCONCLUSIVE"
  | Encode_error _ -> "ENCODE_ERROR"
  | Malformed _ -> "MALFORMED"
  | Unsupported _ -> "UNSUPPORTED"
  | No_status -> "NO_STATUS"

let detail = function
  | Certified | No_status -> ""
  | Refuted s | Inconclusive s | Encode_error s | Malformed s | Unsupported s ->
      s

(* A ship-stopping outcome blocks the gate (DESIGN.md §8 verdict asymmetry). *)
let is_ship_stopper = function Refuted _ -> true | _ -> false
let is_certified = function Certified -> true | _ -> false

(* Exit code for the single-file [certify] subcommand. Malformed vs Unsupported get
   distinct codes (DESIGN task: "exit code distinguishes unsupported from malformed"). *)
let exit_code = function
  | Certified -> 0
  | Refuted _ | Encode_error _ -> 1 (* ship-stopper / encoder bug *)
  | Inconclusive _ -> 2
  | Malformed _ -> 3
  | Unsupported _ -> 4
  | No_status -> 5
