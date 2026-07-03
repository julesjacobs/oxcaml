(* Support module for lean_reflectclient.ml: reflected functions whose
   definitions travel to clients through the .cmi (no .mli, so the cmi
   comes from this implementation and carries the spec export). *)

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t

let rec total_ fib n =
  if n <= 0
  then 0
  else if n = 1
  then 1
  else fib (n - 1) + fib (n - 2)
[@@vox.decreases n]
