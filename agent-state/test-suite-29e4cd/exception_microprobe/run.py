#!/usr/bin/env python3
import os
import shutil
import statistics
import subprocess
import textwrap
import time
from pathlib import Path


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
BUILD = HERE / "build"
SRC = HERE / "src"

OCAMLOPT = Path(os.environ.get("OCAMLOPT", ROOT / "_stage0_stack_llvm_install/bin/ocamlopt.opt"))
OCAMLLIB = Path(os.environ.get("OCAMLLIB", ROOT / "_stage0_stack_llvm_install/lib/ocaml"))
LLVM_PATH = Path(os.environ.get("LLVM_PATH", ROOT.parent / "clang-wrapper"))
LLVM_EXTRA_FLAGS = os.environ.get("LLVM_EXTRA_FLAGS", "")
SAMPLES = int(os.environ.get("SAMPLES", "5"))
WARMUPS = int(os.environ.get("WARMUPS", "1"))

COMMON_FLAGS = [
    "-nostdlib",
    "-I",
    str(OCAMLLIB),
    "-I",
    str(OCAMLLIB / "compiler-libs"),
    "-O3",
    "-unbox-closures",
    "-w",
    "-32-34-38-69",
]


CASES = {
    "raise_caught_same_function": r"""
exception E

let n = 40_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try
        if i land 1 = 0 then raise_notrace E;
        1
      with E -> 3
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "raise_caught_cross_function": r"""
exception E

let n = 40_000_000

let[@inline never] fail_on_even i =
  if i land 1 = 0 then raise_notrace E;
  1

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try fail_on_even i with E -> 3
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "failed_lookup_exception": r"""
exception Miss

type cell = Cell of int * int * cell list

let rec find k = function
  | [] -> raise_notrace Miss
  | Cell (x, y, rest) :: tl ->
      if k = x then y
      else if k < x then find k rest
      else find k tl

let table =
  [ Cell (10, 1, [Cell (11, 2, []); Cell (12, 3, [])])
  ; Cell (20, 4, [Cell (21, 5, []); Cell (22, 6, [])])
  ; Cell (30, 7, [Cell (31, 8, []); Cell (32, 9, [])])
  ]

let n = 18_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    let k = 100 + (i land 7) in
    acc :=
      !acc +
      try find k table with Miss -> i land 3
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "boyer_like_failed_unify": r"""
exception Unify

type term = Var of int | Prop of int * term list
type subst = Bind of int * term

let rec get_binding v = function
  | [] -> raise_notrace Unify
  | Bind (w, t) :: rest -> if v = w then t else get_binding v rest

let rec unify1 term1 term2 subst =
  match term2 with
  | Var v ->
      (try
         if get_binding v subst = term1 then subst else raise_notrace Unify
       with Unify -> Bind (v, term1) :: subst)
  | Prop (head2, argl2) ->
      (match term1 with
       | Var _ -> raise_notrace Unify
       | Prop (head1, argl1) ->
           if head1 = head2 then unify1_lst argl1 argl2 subst else raise_notrace Unify)

and unify1_lst l1 l2 subst =
  match l1, l2 with
  | [], [] -> subst
  | h1 :: r1, h2 :: r2 -> unify1_lst r1 r2 (unify1 h1 h2 subst)
  | _ -> raise_notrace Unify

let rec rewrite_with_lemmas term lemmas =
  match lemmas with
  | [] -> term
  | (t1, t2) :: rest ->
      try
        let subst = unify1 term t1 [] in
        ignore subst;
        t2
      with Unify -> rewrite_with_lemmas term rest

let term = Prop (42, [Prop (1, [Var 0]); Prop (2, [Var 1]); Prop (3, [Var 2])])

let bad_lemmas =
  [ Prop (1, [Var 0]), Var 0
  ; Prop (2, [Var 0]), Var 0
  ; Prop (3, [Var 0]), Var 0
  ; Prop (4, [Var 0]), Var 0
  ; Prop (5, [Var 0]), Var 0
  ; Prop (6, [Var 0]), Var 0
  ; Prop (7, [Var 0]), Var 0
  ; Prop (8, [Var 0]), Var 0
  ]

let n = 7_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    match rewrite_with_lemmas term bad_lemmas with
    | Var x -> acc := !acc + x
    | Prop (h, _) -> acc := !acc + h + (i land 1)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "nested_failed_unify": r"""
exception Unify

type term = Var of int | Prop of int * term list
type subst = Bind of int * term

let rec get_binding v = function
  | [] -> raise_notrace Unify
  | Bind (w, t) :: rest -> if v = w then t else get_binding v rest

let rec unify1 term1 term2 subst =
  match term2 with
  | Var v ->
      (try
         if get_binding v subst = term1 then subst else raise_notrace Unify
       with Unify -> Bind (v, term1) :: subst)
  | Prop (head2, argl2) ->
      (match term1 with
       | Var _ -> raise_notrace Unify
       | Prop (head1, argl1) ->
           if head1 = head2 then unify1_lst argl1 argl2 subst else raise_notrace Unify)

and unify1_lst l1 l2 subst =
  match l1, l2 with
  | [], [] -> subst
  | h1 :: r1, h2 :: r2 -> unify1_lst r1 r2 (unify1 h1 h2 subst)
  | _ -> raise_notrace Unify

let rec try_rules term acc = function
  | [] -> acc
  | (t1, v) :: rest ->
      let acc =
        try
          let subst = unify1 term t1 [] in
          acc + List.length subst + v
        with Unify -> acc + 1
      in
      try_rules term acc rest

let term = Prop (100, [Prop (10, [Var 0; Var 1]); Prop (20, [Var 2; Var 3])])

let rules =
  [ Prop (1, [Var 0]), 1
  ; Prop (2, [Var 0]), 2
  ; Prop (3, [Var 0]), 3
  ; Prop (4, [Var 0]), 4
  ; Prop (5, [Var 0]), 5
  ; Prop (6, [Var 0]), 6
  ; Prop (7, [Var 0]), 7
  ; Prop (8, [Var 0]), 8
  ; Prop (9, [Var 0]), 9
  ; Prop (11, [Var 0]), 11
  ; Prop (12, [Var 0]), 12
  ; Prop (13, [Var 0]), 13
  ]

let n = 6_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + try_rules term (i land 1) rules
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "catch_failure_then_unify": r"""
exception Unify

type term = Var of int | Prop of int * term list
type subst = Bind of int * term

let rec get_binding v = function
  | [] -> failwith "unbound"
  | Bind (w, t) :: rest -> if v = w then t else get_binding v rest

let rec unify1 term1 term2 subst =
  match term2 with
  | Var v ->
      (try
         if get_binding v subst = term1 then subst else raise_notrace Unify
       with Failure _ -> Bind (v, term1) :: subst)
  | Prop (head2, argl2) ->
      (match term1 with
       | Var _ -> raise_notrace Unify
       | Prop (head1, argl1) ->
           if head1 = head2 then unify1_lst argl1 argl2 subst else raise_notrace Unify)

and unify1_lst l1 l2 subst =
  match l1, l2 with
  | [], [] -> subst
  | h1 :: r1, h2 :: r2 -> unify1_lst r1 r2 (unify1 h1 h2 subst)
  | _ -> raise_notrace Unify

let rec rewrite_with_lemmas term lemmas =
  match lemmas with
  | [] -> term
  | (t1, t2) :: rest ->
      try
        let subst = unify1 term t1 [] in
        ignore subst;
        t2
      with Unify -> rewrite_with_lemmas term rest

let term = Prop (42, [Prop (1, [Var 0]); Prop (2, [Var 1]); Prop (3, [Var 2])])
let lemmas =
  [ Prop (1, [Var 0]), Var 0
  ; Prop (2, [Var 0]), Var 0
  ; Prop (3, [Var 0]), Var 0
  ; Prop (4, [Var 0]), Var 0
  ; Prop (5, [Var 0]), Var 0
  ; Prop (6, [Var 0]), Var 0
  ; Prop (7, [Var 0]), Var 0
  ; Prop (8, [Var 0]), Var 0
  ]

let n = 7_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    match rewrite_with_lemmas term lemmas with
    | Var x -> acc := !acc + x
    | Prop (h, _) -> acc := !acc + h + (i land 1)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "raise_payload_caught_cross_function": r"""
exception E of int

let n = 30_000_000

let[@inline never] fail_on_even i =
  if i land 1 = 0 then raise_notrace (E i);
  1

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try fail_on_even i with E x -> x land 3
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "many_handler_live_roots_raise": r"""
exception E

type box = { a : int; b : int; c : int }

let[@inline never] raise_e () = raise_notrace E

let n = 20_000_000

let run () =
  let r1 = { a = 1; b = 2; c = 3 } in
  let r2 = { a = 4; b = 5; c = 6 } in
  let r3 = { a = 7; b = 8; c = 9 } in
  let r4 = { a = 10; b = 11; c = 12 } in
  let r5 = { a = 13; b = 14; c = 15 } in
  let r6 = { a = 16; b = 17; c = 18 } in
  let r7 = { a = 19; b = 20; c = 21 } in
  let r8 = { a = 22; b = 23; c = 24 } in
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try
        raise_e ()
      with E ->
        r1.a + r2.b + r3.c + r4.a + r5.b + r6.c + r7.a + r8.b + (i land 1)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "closure_call_many_handler_live_roots_raise": r"""
exception E

type box = { a : int; b : int; c : int }

let[@inline never] call f = f ()
let[@inline never] make_raiser () = fun () -> raise_notrace E

let n = 12_000_000

let run () =
  let f = make_raiser () in
  let r1 = { a = 1; b = 2; c = 3 } in
  let r2 = { a = 4; b = 5; c = 6 } in
  let r3 = { a = 7; b = 8; c = 9 } in
  let r4 = { a = 10; b = 11; c = 12 } in
  let r5 = { a = 13; b = 14; c = 15 } in
  let r6 = { a = 16; b = 17; c = 18 } in
  let r7 = { a = 19; b = 20; c = 21 } in
  let r8 = { a = 22; b = 23; c = 24 } in
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try
        call f
      with E ->
        r1.a + r2.b + r3.c + r4.a + r5.b + r6.c + r7.a + r8.b + (i land 1)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "large_variant_dispatch": r"""
type t =
  | A0 of int | A1 of int | A2 of int | A3 of int | A4 of int | A5 of int
  | A6 of int | A7 of int | A8 of int | A9 of int | A10 of int | A11 of int
  | A12 of int | A13 of int | A14 of int | A15 of int

let[@inline never] make i =
  match i land 15 with
  | 0 -> A0 i | 1 -> A1 i | 2 -> A2 i | 3 -> A3 i
  | 4 -> A4 i | 5 -> A5 i | 6 -> A6 i | 7 -> A7 i
  | 8 -> A8 i | 9 -> A9 i | 10 -> A10 i | 11 -> A11 i
  | 12 -> A12 i | 13 -> A13 i | 14 -> A14 i | _ -> A15 i

let[@inline never] use = function
  | A0 x -> x + 1 | A1 x -> x + 3 | A2 x -> x + 5 | A3 x -> x + 7
  | A4 x -> x + 11 | A5 x -> x + 13 | A6 x -> x + 17 | A7 x -> x + 19
  | A8 x -> x + 23 | A9 x -> x + 29 | A10 x -> x + 31 | A11 x -> x + 37
  | A12 x -> x + 41 | A13 x -> x + 43 | A14 x -> x + 47 | A15 x -> x + 53

let n = 30_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + use (make i)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "many_live_ints_across_call": r"""
let[@inline never] opaque x = Sys.opaque_identity x
let[@inline never] call i = opaque (i + 1)

let n = 25_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    let a0 = opaque (i + 1) in
    let a1 = opaque (i + 3) in
    let a2 = opaque (i + 5) in
    let a3 = opaque (i + 7) in
    let a4 = opaque (i + 11) in
    let a5 = opaque (i + 13) in
    let a6 = opaque (i + 17) in
    let a7 = opaque (i + 19) in
    let z = call i in
    acc := !acc + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + z
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "closure_env_many_fields": r"""
let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make_closure a0 a1 a2 a3 a4 a5 a6 a7 =
  fun x -> x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7

let n = 25_000_000

let run () =
  let f = make_closure 1 3 5 7 11 13 17 19 in
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + opaque (f i)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "closure_env_sixteen_fields": r"""
let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make_closure a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
  fun x ->
    x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7
    + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15

let n = 20_000_000

let run () =
  let f =
    opaque
      (make_closure
         1 3 5 7 11 13 17 19
         23 29 31 37 41 43 47 53)
  in
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + opaque (f i)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "closure_env_twentyfour_fields": r"""
let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make_closure
    a0 a1 a2 a3 a4 a5 a6 a7
    a8 a9 a10 a11 a12 a13 a14 a15
    a16 a17 a18 a19 a20 a21 a22 a23 =
  fun x ->
    x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7
    + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15
    + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23

let n = 16_000_000

let run () =
  let f =
    opaque
      (make_closure
         1 3 5 7 11 13 17 19
         23 29 31 37 41 43 47 53
         59 61 67 71 73 79 83 89)
  in
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + opaque (f i)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "int_div_mod_loop": r"""
let[@inline never] opaque x = Sys.opaque_identity x

let n = 35_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    let x = opaque (i + 12345) in
    acc := !acc + ((x / 7) mod 97) + ((x / 13) mod 31)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "int_compare_branch_chain": r"""
let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] classify x =
  if x < 10 then 1
  else if x < 20 then 2
  else if x < 30 then 3
  else if x < 40 then 4
  else if x < 50 then 5
  else if x < 60 then 6
  else if x < 70 then 7
  else if x < 80 then 8
  else if x < 90 then 9
  else 10

let n = 50_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + classify (opaque (i land 127))
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "tuple_match_loop": r"""
let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make i =
  opaque (i, i + 1, i + 2, i + 3)

let[@inline never] use t =
  match t with
  | a, b, c, d -> a + (2 * b) + (3 * c) + (4 * d)

let n = 30_000_000

let run () =
  let acc = ref 0 in
  for i = 1 to n do
    acc := !acc + use (make i)
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "closure_env_in_try_no_raise": r"""
exception E

let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make_closure a0 a1 a2 a3 a4 a5 a6 a7 =
  fun x -> x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7

let n = 25_000_000

let run () =
  let f = opaque (make_closure 1 3 5 7 11 13 17 19) in
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try opaque (f i) with E -> i land 7
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "closure_env_in_try_hit": r"""
exception E

let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make_closure a0 a1 a2 a3 a4 a5 a6 a7 =
  fun x ->
    if x land 1 = 0 then raise_notrace E;
    x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7

let n = 20_000_000

let run () =
  let f = opaque (make_closure 1 3 5 7 11 13 17 19) in
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try opaque (f i) with E -> i land 7
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "closure_env_nested_try_no_raise": r"""
exception E1
exception E2
exception E3

let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make_closure a0 a1 a2 a3 a4 a5 a6 a7 =
  fun x -> x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7

let n = 20_000_000

let run () =
  let f = opaque (make_closure 1 3 5 7 11 13 17 19) in
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try
        try
          try opaque (f i) with E3 -> i land 3
        with E2 -> i land 5
      with E1 -> i land 7
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
    "closure_env_nested_try_hit": r"""
exception E1
exception E2
exception E3

let[@inline never] opaque x = Sys.opaque_identity x

let[@inline never] make_closure a0 a1 a2 a3 a4 a5 a6 a7 =
  fun x ->
    if x land 1 = 0 then raise_notrace E3;
    x + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7

let n = 16_000_000

let run () =
  let f = opaque (make_closure 1 3 5 7 11 13 17 19) in
  let acc = ref 0 in
  for i = 1 to n do
    acc :=
      !acc +
      try
        try
          try opaque (f i) with E3 -> i land 3
        with E2 -> i land 5
      with E1 -> i land 7
  done;
  !acc

let () =
  let x = run () in
  if x = 0 then print_endline "bad"
""",
}


def run(cmd, *, cwd=None, env=None):
    start = time.perf_counter()
    subprocess.run([str(x) for x in cmd], cwd=cwd, env=env, check=True)
    return time.perf_counter() - start


def compiler_env():
    env = os.environ.copy()
    env["OCAMLLIB"] = str(OCAMLLIB)
    env["OCAMLPARAM"] = ""
    return env


def write_sources():
    SRC.mkdir(parents=True, exist_ok=True)
    for name, src in CASES.items():
        (SRC / f"{name}.ml").write_text(textwrap.dedent(src).strip() + "\n")


def compile_case(name, mode):
    exe = BUILD / f"{name}.{mode}"
    flags = []
    if mode == "llvm":
        flags = ["-llvm-backend", "-llvm-path", str(LLVM_PATH)]
        if LLVM_EXTRA_FLAGS:
            flags += ["-llvm-flags", LLVM_EXTRA_FLAGS]
    run([OCAMLOPT, *COMMON_FLAGS, *flags, SRC / f"{name}.ml", "-o", exe], env=compiler_env())
    return exe


def time_exe(exe):
    start = time.perf_counter()
    subprocess.check_call([str(exe)], stdout=subprocess.DEVNULL)
    return time.perf_counter() - start


def main():
    if BUILD.exists():
        shutil.rmtree(BUILD)
    BUILD.mkdir(parents=True)
    write_sources()

    for name in CASES:
        exes = {mode: compile_case(name, mode) for mode in ["native", "llvm"]}
        for _ in range(WARMUPS):
            for mode in ["native", "llvm"]:
                time_exe(exes[mode])
        samples = {"native": [], "llvm": []}
        for i in range(SAMPLES):
            order = ["native", "llvm"] if i % 2 == 0 else ["llvm", "native"]
            for mode in order:
                samples[mode].append(time_exe(exes[mode]))
        med = {mode: statistics.median(times) for mode, times in samples.items()}
        ratio = med["llvm"] / med["native"]
        print(f"{name}: native={med['native']:.4f}s llvm={med['llvm']:.4f}s ratio={ratio:.4f}")


if __name__ == "__main__":
    main()
