let[@inline never] black_box_int (x : int) = Sys.opaque_identity x
let[@inline never] black_box_string (x : string) = Sys.opaque_identity x
let[@inline never] black_box x = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 100_000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 10

let print_result x = Printf.printf "%d\n%!" (x land 0x3fffffff)


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
