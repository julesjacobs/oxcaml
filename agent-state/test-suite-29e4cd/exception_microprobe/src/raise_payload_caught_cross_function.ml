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
