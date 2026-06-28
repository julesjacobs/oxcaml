(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -g -llvm-backend";
 native;
*)

type ec = int * int

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE") with Not_found | Failure _ -> 1

let iterations = match test_size with 3 -> 4000 | 2 -> 1000 | _ -> 200
let width = match test_size with 3 -> 97 | 2 -> 73 | _ -> 41

let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] force_collection seed =
  let junk =
    Array.init 32 (fun i ->
        opaque
          ( string_of_int seed,
            [| seed + i; seed + i + 1; seed + i + 2 |],
            fun x -> x + seed + i ))
  in
  ignore (opaque junk);
  if seed land 7 = 0 then Gc.full_major () else Gc.minor ()

let[@inline never] join (a0, a1) (b0, b1) =
  opaque (a0 + b0, a1 + b1)

let[@inline never] mapped seed x =
  force_collection (seed + x);
  opaque (x + seed, (x lxor seed) land 0xff)

let[@inline never] join_list_map xs f =
  match xs with
  | [] -> 0, 0
  | x :: xs -> List.fold_left (fun acc x -> join acc (f x)) (f x) xs

let reference seed xs =
  List.fold_left
    (fun (a0, a1) x -> join (a0, a1) (x + seed, (x lxor seed) land 0xff))
    (0, 0) xs

let check seed =
  let xs = List.init width (fun i -> opaque (seed + i)) in
  let f = opaque (mapped seed) in
  let actual = join_list_map xs f in
  let expected = reference seed xs in
  if actual <> expected
  then
    Printf.ksprintf failwith "closure-call roots: seed=%d actual=%d,%d expected=%d,%d"
      seed (fst actual) (snd actual) (fst expected) (snd expected)

let () =
  for seed = 1 to iterations do
    check seed
  done;
  print_endline "ok"
