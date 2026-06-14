(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -g -llvm-backend";
 native;
*)

exception E of int

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE") with Not_found | Failure _ -> 1

let iterations = match test_size with 3 -> 3000 | 2 -> 1000 | _ -> 200

let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] force_collection seed =
  let junk =
    List.init 32 (fun i ->
        opaque (string_of_int (seed + i), Array.init 5 (fun j -> seed + i + j)))
  in
  ignore (opaque junk);
  if seed land 3 = 0 then Gc.full_major () else Gc.minor ()

let[@inline never] may_raise seed =
  force_collection seed;
  if seed land 1 = 0 then raise_notrace (E seed) else seed + 1

let[@inline never] with_handler seed payload k =
  try
    let n = may_raise seed in
    String.length payload + k n
  with E n ->
    force_collection (n + 17);
    String.length payload + k n + 1000

let check seed =
  let payload =
    opaque ("payload-" ^ string_of_int seed ^ "-" ^ String.make ((seed land 15) + 1) 'p')
  in
  let captured = opaque [| seed; seed + 1; seed + 2; seed + 3 |] in
  let k = opaque (fun n -> Array.unsafe_get captured (n land 3) + n) in
  let actual = with_handler seed payload k in
  let expected =
    String.length payload
    + k (if seed land 1 = 0 then seed else seed + 1)
    + (if seed land 1 = 0 then 1000 else 0)
  in
  if actual <> expected
  then Printf.ksprintf failwith "trap roots: seed=%d actual=%d expected=%d" seed actual expected

let () =
  for seed = 1 to iterations do
    check seed
  done;
  print_endline "ok"
