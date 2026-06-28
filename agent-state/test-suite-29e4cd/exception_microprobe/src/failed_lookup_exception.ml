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
