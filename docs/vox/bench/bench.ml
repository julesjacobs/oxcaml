(* Benchmark for demo/lean_kernel.ml: the verified, checkless [sum]
   and [dot] against the bounds-checked stdlib loops, and against the
   same code written with [unsafe_get] by hand (the verified kernel
   should match it exactly: verification is free at runtime).
   Compiled natively, UNDER verification, by run.sh. *)

let n = 10_000_000
let a = Iarray.init n (fun i -> i land 1023)
let b = Iarray.init n (fun i -> (i * 7) land 1023)


let sum_checked (a : int iarray) =
  let n = Iarray.length a in
  let rec go i acc = if i < n then go (i + 1) (acc + Iarray.get a i) else acc in
  go 0 0

let sum_unsafe (a : int iarray) =
  let n = Iarray.length a in
  let rec go i acc =
    if i < n then go (i + 1) (acc + Iarray.unsafe_get a i) else acc
  in
  go 0 0

let dot_checked (a : int iarray) (b : int iarray) =
  let n = Iarray.length a in
  let rec go i acc =
    if i < n then go (i + 1) (acc + (Iarray.get a i * Iarray.get b i)) else acc
  in
  go 0 0

let time name f =
  let reps = 9 in
  let best = ref infinity in
  let result = ref 0 in
  for _ = 1 to reps do
    let t0 = Sys.time () in
    result := f ();
    let dt = Sys.time () -. t0 in
    if dt < !best then best := dt
  done;
  Printf.printf "%-24s %8.2f ms   result=%d\n"
    name (!best *. 1e3) !result;
  !best

let () =
  Printf.printf "n = %d ints, best of 9 runs\n" n;
  let sc = time "sum, bounds-checked" (fun () -> sum_checked a) in
  let sv = time "sum, VERIFIED unsafe" (fun () -> Lean_kernel.sum a) in
  let sh = time "sum, unsafe by hand" (fun () -> sum_unsafe a) in

  Printf.printf "\nsum: verified is %.2fx the checked loop (hand-unsafe: %.2fx)\n"
    (sc /. sv) (sc /. sh);
  (* [dot]'s contract demands [len a <= len b]; the DRIVER proves it
     like any other client -- this very benchmark failed to compile
     until it did. *)
  let la = Lean_kernel.length a in
  let lb = Lean_kernel.length b in
  if la <= lb then begin
    let dc = time "dot, bounds-checked" (fun () -> dot_checked a b) in
    let dv = time "dot, VERIFIED unsafe" (fun () -> Lean_kernel.dot a b) in
    Printf.printf "dot: verified is %.2fx the checked loop\n" (dc /. dv)
  end
  else print_string "arrays of incompatible length\n"
