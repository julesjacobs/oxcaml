let n = 128
let rounds = 80

let[@inline never] init_a i =
  float_of_int ((i * 13 + 5) land 255) *. 0.00390625

let[@inline never] init_b i =
  float_of_int ((i * 29 + 11) land 255) *. 0.00390625

let a = Array.init (n * n) init_a
let b = Array.init (n * n) init_b
let c = Array.make (n * n) 0.0

let[@inline never] run () =
  for r = 1 to rounds do
    for i = 0 to n - 1 do
      let row = i * n in
      for j = 0 to n - 1 do
        let acc = ref (float_of_int r *. 0.000001) in
        for k = 0 to n - 1 do
          acc :=
            !acc
            +. Array.unsafe_get a (row + k)
               *. Array.unsafe_get b ((k * n) + j)
        done;
        Array.unsafe_set c (row + j) !acc
      done
    done
  done;
  Array.unsafe_get c ((n * n) - 1)

let () =
  Printf.printf "%.6f\n" (run ())
