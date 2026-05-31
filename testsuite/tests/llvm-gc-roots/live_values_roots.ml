(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -g -llvm-backend";
 native;
*)

type node =
  { id : int;
    name : string;
    data : int array;
    apply : int -> int;
    mutable next : node option
  }

let test_size =
  try int_of_string (Sys.getenv "OCAML_TEST_SIZE") with Not_found | Failure _ -> 1

let iterations = match test_size with 3 -> 2000 | 2 -> 700 | _ -> 150
let width = match test_size with 3 -> 191 | 2 -> 127 | _ -> 67

let[@inline never] opaque x = Sys.opaque_identity x

let make_node id =
  let name = "node-" ^ string_of_int id ^ "-" ^ String.make ((id land 7) + 1) 'x' in
  let data = Array.init 9 (fun i -> id * 17 + i) in
  let apply x = x + id + String.length name + Array.unsafe_get data (x mod 9) in
  opaque { id; name; data; apply; next = None }

let nodes = Array.init width make_node

let () =
  for i = 0 to width - 1 do
    nodes.(i).next <- Some nodes.((i + 1) mod width)
  done

let[@inline never] force_collection seed =
  let live_noise =
    Array.init 24 (fun i ->
        opaque
          { id = seed + i;
            name = String.make ((seed + i) land 15) (Char.chr (65 + ((seed + i) mod 26)));
            data = [| seed; i; seed + i |];
            apply = (fun x -> x lxor seed lxor i);
            next = None
          })
  in
  ignore (opaque live_noise);
  match seed mod 11 with
  | 0 -> Gc.compact ()
  | 1 | 2 | 3 -> Gc.full_major ()
  | _ -> Gc.minor ()

let[@inline never] checksum_node n seed =
  let next_id =
    match n.next with
    | None -> -1
    | Some next -> next.id
  in
  n.id + String.length n.name + Array.unsafe_get n.data (seed mod 9)
  + n.apply (seed land 7) + next_id

let check seed =
  let a = opaque nodes.(seed mod width) in
  let b = opaque nodes.((seed * 17 + 3) mod width) in
  let c = opaque nodes.((seed * 31 + 5) mod width) in
  let before = checksum_node a seed + checksum_node b (seed + 1) + checksum_node c (seed + 2) in
  force_collection seed;
  let after = checksum_node a seed + checksum_node b (seed + 1) + checksum_node c (seed + 2) in
  if before <> after
  then Printf.ksprintf failwith "live-value roots: seed=%d before=%d after=%d" seed before after

let () =
  for seed = 1 to iterations do
    check seed
  done;
  print_endline "ok"
