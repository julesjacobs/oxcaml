(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-vox-dump-vc -c";
 compiler_output = "direct_if_total_call_vc.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/direct_if_total_call_vc.reference";
 check-ocamlc.byte-output;
*)

external int_equal : int -> int -> bool @@ total = "%equal"

let (is_zero @ total) (x : int) : bool{ _ = int_equal x 0 } =
  int_equal x 0

let direct x =
  if is_zero x then (x : int{ _ = 0 }) else 0

let bound x =
  let result = is_zero x in
  if result then (x : int{ _ = 0 }) else 0

external partial_is_zero : x:int -> bool{ _ = int_equal x 0 }
  = "vox_partial_is_zero"

let partial x =
  if partial_is_zero ~x then (x : int{ _ = 0 }) else 0

let partial_alias = partial_is_zero

let partial_via_alias x =
  if partial_alias ~x then (x : int{ _ = 0 }) else 0

let unreachable () : int{ false } =
  if Sys.opaque_identity (raise Exit) then 0 else 1

let reachable_short_circuit () : int{ false } =
  if true || raise Exit then 0 else 1

let established_key = Sys.opaque_identity 0

external establish_key : unit -> unit{ established_key = 7 } @@ total
  = "%identity"

let ordered flag =
  if (ignore (establish_key ()); flag)
  then (established_key : int{ _ = 7 })
  else (established_key : int{ _ = 7 })
