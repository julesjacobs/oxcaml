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
