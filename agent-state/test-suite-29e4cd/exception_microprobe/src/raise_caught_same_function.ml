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
