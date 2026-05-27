#!/usr/bin/env python3
import json
import math
import os
import re
import statistics
import subprocess
import textwrap
import time
from pathlib import Path


SCRIPT_DIR = Path(__file__).resolve().parent
BUILD = SCRIPT_DIR / ".build"
SRC = BUILD / "src"
INSPECT = BUILD / "inspect"
OCAMLOPT = Path(os.environ["OCAMLOPT"])
OCAMLLIB = Path(os.environ["OCAMLLIB"])
LLVM_PATH = os.environ["LLVM_PATH"]
PAIRS = int(os.environ.get("PAIRS", "3"))


COMMON = r'''
let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)
'''


CASES = {
    "string_assoc_find_hit": {
        "params": (600_000, 20),
        "source": r'''
let[@inline never] find key a =
  let rec loop i =
    if i = Array.length a then -1
    else
      let k, v = Array.unsafe_get a i in
      if String.equal key k then v else loop (i + 1)
  in
  loop 0

let[@inline never] run n reps =
  let a =
    Array.init 32 (fun i -> "compiler_assoc_key_" ^ string_of_int i, i)
  in
  let keys = Array.init 32 (fun i -> fst (Array.unsafe_get a i)) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      let key = Array.unsafe_get keys ((i + r) land 31) in
      acc := !acc + find key a
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "string_tree_prefix_heavy": {
        "params": (500_000, 20),
        "source": r'''
type tree = Empty | Node of tree * string * int * tree

let key i =
  "LongIdent.Module.Submodule.SharedPrefix.component_name_" ^ string_of_int i

let rec build lo hi =
  if lo > hi then Empty
  else
    let mid = (lo + hi) / 2 in
    Node (build lo (mid - 1), key mid, mid, build (mid + 1) hi)

let[@inline never] rec find key = function
  | Empty -> -1
  | Node (left, name, value, right) ->
    let c = String.compare key name in
    if c = 0 then value else if c < 0 then find key left else find key right

let[@inline never] run n reps =
  let tree = build 0 63 in
  let keys = Array.init 64 key in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find (Array.unsafe_get keys ((i + r) land 63)) tree
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "string_tree_first_byte_diff": {
        "params": (700_000, 20),
        "source": r'''
type tree = Empty | Node of tree * string * int * tree

let key i =
  String.make 1 (Char.chr (65 + (i land 25))) ^ "_compiler_key_" ^ string_of_int i

let rec build lo hi =
  if lo > hi then Empty
  else
    let mid = (lo + hi) / 2 in
    Node (build lo (mid - 1), key mid, mid, build (mid + 1) hi)

let[@inline never] rec find key = function
  | Empty -> -1
  | Node (left, name, value, right) ->
    let c = String.compare key name in
    if c = 0 then value else if c < 0 then find key left else find key right

let[@inline never] run n reps =
  let tree = build 0 63 in
  let keys = Array.init 64 key in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find (Array.unsafe_get keys ((i + r) land 63)) tree
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "string_map_interned_keys": {
        "params": (500_000, 20),
        "source": r'''
module M = Map.Make (String)

let[@inline never] run n reps =
  let keys = Array.init 64 (fun i -> "compiler_interned_key_" ^ string_of_int i) in
  let map =
    Array.fold_left (fun m k -> M.add k (String.length k) m) M.empty keys
  in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      let key = Array.unsafe_get keys ((i + r) land 63) in
      acc := !acc + M.find key map
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "string_map_equal_content": {
        "params": (450_000, 20),
        "source": r'''
module M = Map.Make (String)

let fresh s = Bytes.to_string (Bytes.of_string s)

let[@inline never] run n reps =
  let stored = Array.init 64 (fun i -> "compiler_equal_key_" ^ string_of_int i) in
  let lookup = Array.map fresh stored in
  let map =
    Array.fold_left (fun m k -> M.add k (String.length k) m) M.empty stored
  in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      let key = Array.unsafe_get lookup ((i + r) land 63) in
      acc := !acc + M.find key map
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "string_equal_guarded_dispatch": {
        "params": (1_000_000, 20),
        "source": r'''
let[@inline never] dispatch s =
  if String.equal s "[@@inline]" then 1
  else if String.equal s "[@@specialise]" then 2
  else if String.equal s "[@@local]" then 3
  else if String.equal s "[@@zero_alloc]" then 4
  else if String.equal s "[@@poll]" then 5
  else 6

let[@inline never] run n reps =
  let keys =
    [| "[@@inline]"; "[@@specialise]"; "[@@local]"; "[@@zero_alloc]";
       "[@@poll]"; "[@@other]" |]
  in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + dispatch (Array.unsafe_get keys ((i + r) mod 6))
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "try_find_hit_deep": {
        "params": (700_000, 20),
        "source": r'''
exception Miss

let[@inline never] rec scan key a i =
  if i = Array.length a then raise Miss
  else if Array.unsafe_get a i = key then i
  else scan key a (i + 1)

let[@inline never] find1 key a = scan key a 0
let[@inline never] find2 key a = find1 key a
let[@inline never] find3 key a = find2 key a

let[@inline never] run n reps =
  let a = Array.init 16 (fun i -> i * 2) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      try acc := !acc + find3 (((i + r) land 15) lsl 1) a
      with Miss -> decr acc
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "try_find_multiple_handlers": {
        "params": (650_000, 20),
        "source": r'''
exception Miss
exception Outer

let[@inline never] rec scan key a i =
  if i = Array.length a then raise Miss
  else if Array.unsafe_get a i = key then i
  else scan key a (i + 1)

let[@inline never] run n reps =
  let a = Array.init 16 (fun i -> i * 2) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      try
        try acc := !acc + scan (((i + r) land 15) lsl 1) a 0
        with Miss -> acc := !acc - 1
      with Outer -> acc := !acc - 2
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "try_find_cold_handler_large_body": {
        "params": (850_000, 20),
        "source": r'''
exception Miss

let[@inline never] rec scan key a i =
  if i = Array.length a then raise Miss
  else if Array.unsafe_get a i = key then i
  else scan key a (i + 1)

let[@inline never] cold_handler x =
  let acc = ref x in
  for i = 1 to 100 do
    acc := (!acc * 33 + i) land 0x3fffffff
  done;
  !acc

let[@inline never] run n reps =
  let a = Array.init 16 (fun i -> i * 2) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      try acc := !acc + scan (((i + r) land 15) lsl 1) a 0
      with Miss -> acc := !acc + cold_handler i
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "try_find_miss_rare": {
        "params": (900_000, 20),
        "source": r'''
exception Miss

let[@inline never] rec scan key a i =
  if i = Array.length a then raise Miss
  else if Array.unsafe_get a i = key then i
  else scan key a (i + 1)

let[@inline never] run n reps =
  let a = Array.init 16 (fun i -> i * 2) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      let key = if i land 255 = 0 then -1 else (((i + r) land 15) lsl 1) in
      try acc := !acc + scan key a 0
      with Miss -> acc := (!acc + i) land 0x3fffffff
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "try_with_string_compare_hit": {
        "params": (400_000, 20),
        "source": r'''
exception Miss
type tree = Empty | Node of tree * string * int * tree

let key i = "LongIdent.Module.try_lookup_shared_prefix_" ^ string_of_int i

let rec build lo hi =
  if lo > hi then Empty
  else
    let mid = (lo + hi) / 2 in
    Node (build lo (mid - 1), key mid, mid, build (mid + 1) hi)

let[@inline never] rec find key = function
  | Empty -> raise Miss
  | Node (left, name, value, right) ->
    let c = String.compare key name in
    if c = 0 then value else if c < 0 then find key left else find key right

let[@inline never] run n reps =
  let tree = build 0 63 in
  let keys = Array.init 64 key in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      try acc := !acc + find (Array.unsafe_get keys ((i + r) land 63)) tree
      with Miss -> decr acc
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "env_find_same_mini": {
        "params": (450_000, 20),
        "source": r'''
exception Not_found_same
type entry = { name : string; stamp : int; value : int }

let[@inline never] rec find_same name stamp entries i =
  if i = Array.length entries then raise Not_found_same
  else
    let e = Array.unsafe_get entries i in
    if e.stamp = stamp && String.equal e.name name then e.value
    else find_same name stamp entries (i + 1)

let[@inline never] run n reps =
  let entries =
    Array.init 32 (fun i ->
      { name = "Env.Longident.SharedPrefix.name_" ^ string_of_int i;
        stamp = i;
        value = i * 3 })
  in
  let names = Array.map (fun e -> e.name) entries in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      let idx = (i + r) land 31 in
      try acc := !acc + find_same (Array.unsafe_get names idx) idx entries 0
      with Not_found_same -> decr acc
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "nested_scope_lookup": {
        "params": (350_000, 20),
        "source": r'''
module M = Map.Make (String)

let[@inline never] rec find_scopes key scopes i =
  if i = Array.length scopes then -1
  else
    match M.find_opt key (Array.unsafe_get scopes i) with
    | Some v -> v
    | None -> find_scopes key scopes (i + 1)

let[@inline never] run n reps =
  let scopes =
    Array.init 5 (fun s ->
      List.init 24 (fun i ->
        "scope_" ^ string_of_int s ^ "_shared_lookup_key_" ^ string_of_int i,
        s * 100 + i)
      |> List.fold_left (fun m (k, v) -> M.add k v m) M.empty)
  in
  let keys =
    Array.init 24 (fun i -> "scope_0_shared_lookup_key_" ^ string_of_int i)
  in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find_scopes (Array.unsafe_get keys ((i + r) mod 24)) scopes 0
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "persistent_map_update_lookup": {
        "params": (100_000, 15),
        "source": r'''
module M = Map.Make (String)

let[@inline never] run n reps =
  let keys = Array.init 128 (fun i -> "persistent_key_" ^ string_of_int i) in
  let acc = ref 0 in
  for r = 1 to reps do
    let map = ref M.empty in
    for i = 1 to n do
      let key = Array.unsafe_get keys ((i + r) land 127) in
      map := M.add key i !map;
      acc := !acc + M.find key !map
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "closure_call_in_try_hit": {
        "params": (3_000_000, 15),
        "source": r'''
exception Miss
let[@inline never] inc x = x + 1
let[@inline never] call f x = f x

let[@inline never] run n reps =
  let f = black_box inc in
  let acc = ref 0 in
  for _ = 1 to reps do
    for i = 1 to n do
      try acc := call f (!acc + i)
      with Miss -> decr acc
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "direct_call_in_try_hit": {
        "params": (3_000_000, 15),
        "source": r'''
exception Miss
let[@inline never] inc x = x + 1

let[@inline never] run n reps =
  let acc = ref 0 in
  for _ = 1 to reps do
    for i = 1 to n do
      try acc := inc (!acc + i)
      with Miss -> decr acc
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "higher_order_fold_string_keys": {
        "params": (300_000, 20),
        "source": r'''
let[@inline never] pred key value =
  if String.equal key "fold_shared_key_17" then value + 1 else value

let[@inline never] run n reps =
  let entries = Array.init 64 (fun i -> "fold_shared_key_" ^ string_of_int i, i) in
  let acc = ref 0 in
  for r = 1 to reps do
    for _ = 1 to n do
      Array.iter (fun (k, v) -> acc := !acc + pred k (v + r)) entries
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "record_mutate_old_to_immediate": {
        "params": (4_000_000, 20),
        "source": r'''
type t = { mutable x : int; mutable y : int }

let[@inline never] run n reps =
  let r = { x = 0; y = 1 } in
  let acc = ref 0 in
  for j = 1 to reps do
    for i = 1 to n do
      r.x <- i + j;
      acc := !acc + r.x
    done
  done;
  !acc + r.y

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "record_mutate_old_to_young": {
        "params": (700_000, 15),
        "source": r'''
type t = { mutable x : int option }

let[@inline never] run n reps =
  let r = { x = None } in
  let acc = ref 0 in
  for j = 1 to reps do
    for i = 1 to n do
      r.x <- Some (i + j);
      match r.x with Some x -> acc := !acc + x | None -> ()
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "array_set_young_values": {
        "params": (600_000, 15),
        "source": r'''
let[@inline never] run n reps =
  let a = Array.make 128 None in
  let acc = ref 0 in
  for j = 1 to reps do
    for i = 1 to n do
      let v = Some (i + j) in
      Array.unsafe_set a (i land 127) v;
      match Array.unsafe_get a (i land 127) with
      | Some x -> acc := !acc + x
      | None -> ()
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "ref_option_churn": {
        "params": (1_000_000, 20),
        "source": r'''
let[@inline never] run n reps =
  let r = ref None in
  let acc = ref 0 in
  for j = 1 to reps do
    for i = 1 to n do
      r := Some (i + j);
      match !r with Some x -> acc := !acc + x | None -> ()
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "list_lookup_string_compare": {
        "params": (500_000, 20),
        "source": r'''
let[@inline never] rec find key = function
  | [] -> -1
  | (k, v) :: rest ->
    let c = String.compare key k in
    if c = 0 then v else find key rest

let[@inline never] run n reps =
  let xs =
    List.init 32 (fun i -> "list_lookup_shared_key_" ^ string_of_int i, i)
  in
  let keys = Array.of_list (List.map fst xs) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find (Array.unsafe_get keys ((i + r) land 31)) xs
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "array_binary_search_string": {
        "params": (700_000, 20),
        "source": r'''
let[@inline never] find key a =
  let rec loop lo hi =
    if lo > hi then -1
    else
      let mid = (lo + hi) lsr 1 in
      let k, v = Array.unsafe_get a mid in
      let c = String.compare key k in
      if c = 0 then v else if c < 0 then loop lo (mid - 1) else loop (mid + 1) hi
  in
  loop 0 (Array.length a - 1)

let[@inline never] run n reps =
  let a =
    Array.init 64 (fun i -> "array_binary_key_" ^ string_of_int i, i)
  in
  let keys = Array.map fst a in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find (Array.unsafe_get keys ((i + r) land 63)) a
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "hash_lookup_string_equal": {
        "params": (800_000, 20),
        "source": r'''
let[@inline never] find key buckets =
  let bucket = Array.unsafe_get buckets 0 in
  let rec loop = function
    | [] -> -1
    | (k, v) :: rest -> if String.equal key k then v else loop rest
  in
  loop bucket

let[@inline never] run n reps =
  let entries =
    List.init 32 (fun i -> "hash_collision_key_" ^ string_of_int i, i)
  in
  let buckets = [| entries |] in
  let keys = Array.of_list (List.map fst entries) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find (Array.unsafe_get keys ((i + r) land 31)) buckets
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "variant_dispatch_with_string_payload": {
        "params": (900_000, 20),
        "source": r'''
type t = Attr of string | Prim of string | Other of int

let[@inline never] eval = function
  | Attr s -> if String.equal s "inline" then 1 else 2
  | Prim s -> if String.compare s "caml_array_get" = 0 then 3 else 4
  | Other i -> i land 7

let[@inline never] run n reps =
  let xs =
    [| Attr "inline"; Attr "local"; Prim "caml_array_get";
       Prim "caml_string_compare"; Other 17 |]
  in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + eval (Array.unsafe_get xs ((i + r) mod 5))
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "variant_dispatch_with_int_payload": {
        "params": (4_000_000, 20),
        "source": r'''
type t = Attr of int | Prim of int | Other of int

let[@inline never] eval = function
  | Attr x -> if x = 17 then 1 else 2
  | Prim x -> if x = 42 then 3 else 4
  | Other i -> i land 7

let[@inline never] run n reps =
  let xs =
    [| Attr 17; Attr 19; Prim 42; Prim 43; Other 17 |]
  in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + eval (Array.unsafe_get xs ((i + r) mod 5))
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "variant_dispatch_with_int_payload_inline": {
        "params": (4_000_000, 20),
        "source": r'''
type t = Attr of int | Prim of int | Other of int

let eval = function
  | Attr x -> if x = 17 then 1 else 2
  | Prim x -> if x = 42 then 3 else 4
  | Other i -> i land 7

let[@inline never] run n reps =
  let xs =
    [| Attr 17; Attr 19; Prim 42; Prim 43; Other 17 |]
  in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + eval (Array.unsafe_get xs ((i + r) mod 5))
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "int_tree_find_same_shape": {
        "params": (1_200_000, 20),
        "source": r'''
type tree = Empty | Node of tree * int * int * tree

let rec build lo hi =
  if lo > hi then Empty
  else
    let mid = (lo + hi) / 2 in
    Node (build lo (mid - 1), mid, mid, build (mid + 1) hi)

let[@inline never] rec find key = function
  | Empty -> -1
  | Node (left, k, v, right) ->
    if key = k then v else if key < k then find key left else find key right

let[@inline never] run n reps =
  let tree = build 0 63 in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      acc := !acc + find ((i + r) land 63) tree
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
    "try_int_find_hit": {
        "params": (800_000, 20),
        "source": r'''
exception Miss

let[@inline never] rec find key a i =
  if i = Array.length a then raise Miss
  else if Array.unsafe_get a i = key then i
  else find key a (i + 1)

let[@inline never] run n reps =
  let a = Array.init 16 (fun i -> i * 2) in
  let acc = ref 0 in
  for r = 1 to reps do
    for i = 1 to n do
      try acc := !acc + find (((i + r) land 15) lsl 1) a 0
      with Miss -> decr acc
    done
  done;
  !acc

let () = print_result (run (black_box_int n) (black_box_int reps))
''',
    },
}


def selected_cases():
    selected = os.environ.get("CASES", "")
    if not selected:
        return list(CASES)
    names = [name for name in selected.split(",") if name]
    missing = [name for name in names if name not in CASES]
    if missing:
        raise SystemExit(f"unknown case(s): {', '.join(missing)}")
    return names


def compile_case(name, mode, extra_flags):
    cmd = [
        str(OCAMLOPT),
        "-nostdlib",
        "-I",
        str(OCAMLLIB),
        "-I",
        str(OCAMLLIB / "compiler-libs"),
        "-O3",
        "-unbox-closures",
        *extra_flags,
        str(SRC / f"{name}.ml"),
        "-o",
        str(BUILD / f"{name}.{mode}"),
    ]
    env = os.environ.copy()
    env["OCAMLLIB"] = str(OCAMLLIB)
    subprocess.run(cmd, env=env, check=True)


def compile_asm(name, mode, extra_flags):
    out = INSPECT / f"{name}.{mode}.s"
    for suffix in [".cmx", ".cmi", ".o", ".s"]:
        path = SRC / f"{name}{suffix}"
        if path.exists():
            path.unlink()
    cmd = [
        str(OCAMLOPT),
        "-nostdlib",
        "-I",
        str(OCAMLLIB),
        "-I",
        str(OCAMLLIB / "compiler-libs"),
        "-O3",
        "-unbox-closures",
        "-S",
        "-c",
        *extra_flags,
        f"{name}.ml",
    ]
    env = os.environ.copy()
    env["OCAMLLIB"] = str(OCAMLLIB)
    subprocess.run(cmd, cwd=SRC, env=env, check=True)
    produced = SRC / f"{name}.s"
    produced.replace(out)
    for suffix in [".cmx", ".cmi", ".o"]:
        path = SRC / f"{name}{suffix}"
        if path.exists():
            path.unlink()
    return out


def count_asm(path):
    text = path.read_text(errors="replace")
    return {
        "lines": text.count("\n") + 1,
        "c_call_wrapper_refs": text.count("c_call_wrapper"),
        "wrap_try_refs": text.count("_wrap_try"),
        "recover_rbp_refs": text.count("recover_rbp"),
        "string_compare_refs": text.count("caml_string_compare"),
        "string_equal_refs": text.count("caml_string_equal"),
        "branch_refs": len(re.findall(r"\tb\.|\tb\t|\tcb|\ttb", text)),
    }


def compatible(a, b):
    return a == b


def main():
    BUILD.mkdir(parents=True, exist_ok=True)
    SRC.mkdir(parents=True, exist_ok=True)
    INSPECT.mkdir(parents=True, exist_ok=True)
    names = selected_cases()
    for name in names:
        source = COMMON + "\n" + CASES[name]["source"]
        (SRC / f"{name}.ml").write_text(textwrap.dedent(source).strip() + "\n")

    results = {}
    for name in names:
        n, reps = CASES[name]["params"]
        compile_case(name, "native", [])
        compile_case(name, "llvm", ["-llvm-backend", "-llvm-path", LLVM_PATH])
        native_asm = compile_asm(name, "native", [])
        llvm_asm = compile_asm(name, "llvm", ["-llvm-backend", "-llvm-path", LLVM_PATH])
        exes = {
            "native": BUILD / f"{name}.native",
            "llvm": BUILD / f"{name}.llvm",
        }
        outputs = {
            mode: subprocess.check_output([str(exe), str(n), str(reps)], text=True).strip()
            for mode, exe in exes.items()
        }
        if not compatible(outputs["native"], outputs["llvm"]):
            raise SystemExit(f"{name}: output mismatch {outputs}")

        samples = {"native": [], "llvm": []}
        for i in range(PAIRS):
            order = ["native", "llvm"] if i % 2 == 0 else ["llvm", "native"]
            for mode in order:
                start = time.perf_counter()
                subprocess.run(
                    [str(exes[mode]), str(n), str(reps)],
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    check=True,
                )
                samples[mode].append(time.perf_counter() - start)

        med = {mode: statistics.median(values) for mode, values in samples.items()}
        ratio = med["llvm"] / med["native"]
        results[name] = {
            "n": n,
            "reps": reps,
            "output": outputs["native"],
            "samples": samples,
            "median": med,
            "ratio_llvm_over_native": ratio,
            "asm": {
                "native": count_asm(native_asm),
                "llvm": count_asm(llvm_asm),
            },
        }
        print(
            f"{name}: native={med['native']:.4f}s "
            f"llvm={med['llvm']:.4f}s ratio={ratio:.4f} "
            f"wrappers={results[name]['asm']['llvm']['c_call_wrapper_refs']} "
            f"wrap_try={results[name]['asm']['llvm']['wrap_try_refs']}",
            flush=True,
        )

    ratios = [results[name]["ratio_llvm_over_native"] for name in names]
    results["_aggregate"] = {
        "cases": len(names),
        "geomean_ratio_llvm_over_native": math.exp(
            sum(math.log(ratio) for ratio in ratios) / len(ratios)
        ),
        "median_ratio_llvm_over_native": statistics.median(ratios),
        "max_ratio_llvm_over_native": max(ratios),
        "min_ratio_llvm_over_native": min(ratios),
    }
    out = BUILD / "summary.json"
    out.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n")
    print(f"SUMMARY_JSON={out}")


if __name__ == "__main__":
    main()
