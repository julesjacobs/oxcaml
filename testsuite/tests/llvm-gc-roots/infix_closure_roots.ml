(* TEST
 macos;
 arch_arm64;
 readonly_files = "infix_closure_roots.sh";
 script = "sh ${test_source_directory}/infix_closure_roots.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE") with Not_found | Failure _ -> 1

let iterations = match test_size with 3 -> 4000 | 2 -> 1000 | _ -> 200

let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] force_collection seed =
  let junk =
    Array.init 64 (fun i ->
        opaque
          ( string_of_int (seed + i),
            [| seed; seed + i; seed lxor i |],
            fun x -> x + seed + i ))
  in
  ignore (opaque junk);
  if seed land 3 = 0 then Gc.full_major () else Gc.minor ()

let[@inline never] call_after_gc f x =
  force_collection x;
  f x

let make_pair seed =
  let salt = opaque (seed * 17 + 5) in
  let rec first x = opaque (x + salt)
  and second x =
    let f = opaque first in
    force_collection (seed + x);
    opaque (f x + salt + 1)
  in
  opaque (first, second)

let check seed =
  let first, second = make_pair seed in
  let f = if seed land 1 = 0 then first else second in
  let actual = call_after_gc f seed in
  let expected =
    if seed land 1 = 0
    then seed + (seed * 17 + 5)
    else seed + (2 * (seed * 17 + 5)) + 1
  in
  if actual <> expected
  then
    Printf.ksprintf failwith "infix closure root: seed=%d actual=%d expected=%d"
      seed actual expected

let () =
  let control = Gc.get () in
  Gc.set
    { control with
      minor_heap_size = min control.minor_heap_size 32768;
      space_overhead = 1
    };
  for seed = 1 to iterations do
    check seed
  done;
  print_endline "ok"
