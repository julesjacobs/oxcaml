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
