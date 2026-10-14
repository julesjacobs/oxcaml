type result =
  { validity : Vox_smt.validity;
    resources : int option;
    core : Vox_smt.Symbol.t list option;
    encoding_seconds : float;
    solving_seconds : float
  }

exception Cancelled

(** Solve a typed compiler query directly. The injected clock is monotonic
    and uses seconds. Validation, encoding, solving and scalar model/core
    retrieval share one resource limit and deadline. Callback exceptions
    propagate unchanged. A fresh term arena and solver belong to each call.
    [request_model] defaults to [true]; [false] skips scalar model retrieval. *)
val check :
  ?request_model:bool ->
  ?cancelled:(unit -> bool) ->
  ?resource_limit:int ->
  ?assumptions:Vox_smt.Symbol.t list ->
  clock:(unit -> float) -> timeout_ms:int -> int_width:int ->
  Vox_smt.query -> result
