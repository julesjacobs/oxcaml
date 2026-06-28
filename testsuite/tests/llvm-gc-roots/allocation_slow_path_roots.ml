(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -g -llvm-backend";
 native;
*)

type payload =
  { tag : string;
    values : int array;
    apply : int -> int
  }

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE") with Not_found | Failure _ -> 1

let iterations = match test_size with 3 -> 1200 | 2 -> 500 | _ -> 120
let rounds = match test_size with 3 -> 700 | 2 -> 400 | _ -> 180

let[@inline never] opaque x = Sys.opaque_identity x

let () =
  let control = Gc.get () in
  Gc.set
    { control with
      minor_heap_size = min control.minor_heap_size 32768;
      space_overhead = 1
    }

let make_payload seed =
  let values = Array.init 17 (fun i -> seed * 19 + i) in
  let tag = "alloc-root-" ^ string_of_int seed ^ "-" ^ String.make ((seed land 7) + 1) 'a' in
  let apply x = Array.unsafe_get values (x mod Array.length values) + String.length tag + x in
  opaque { tag; values; apply }

let[@inline never] checksum payload seed =
  String.length payload.tag
  + Array.unsafe_get payload.values (seed mod Array.length payload.values)
  + payload.apply (seed land 15)

let[@inline never] allocate_churn payload seed =
  let before = checksum payload seed in
  let total = ref 0 in
  for i = 0 to rounds do
    let words = Array.init 64 (fun j -> opaque (seed + i + j)) in
    let text = String.make ((seed + i) land 31) (Char.chr (65 + ((seed + i) mod 26))) in
    total := !total + Array.unsafe_get words (i land 63) + String.length text + payload.apply i
  done;
  let after = checksum payload seed in
  if before <> after
  then
    Printf.ksprintf failwith "allocation roots: seed=%d before=%d after=%d total=%d"
      seed before after !total;
  !total + after

let check seed =
  let payload = make_payload seed in
  ignore (opaque (allocate_churn payload seed))

let () =
  for seed = 1 to iterations do
    check seed
  done;
  print_endline "ok"
