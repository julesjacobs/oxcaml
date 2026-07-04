(* Verified in-place merge: fill a temporary array with the merge of
   the two runs (reading the loan, writing a second loan over the
   temporary), copy it back, and resolve.  Every read and write is a
   slice_lib operation; both loops are recursive contracts whose
   invariants discharge through the fill_inv/copy_step lemmas of the
   interface's prelude.  Nothing here is assumed. *)

open Slice_lib

(* Fill phase: [i] elements of the left run and [j - k] of the right
   run are already merged into [t]'s prefix; each step consumes the
   smaller head (left-biased on ties, like [merged]) and extends the
   prefix.  The source loan is only read, so its [now] threads
   unchanged. *)
let rec fill_loop :
  (k : int{ 0 <= _ }) ->
  (m : slice{ k <= len (now _) }) @ local unique ->
  (n : int{ _ = len (now m) }) ->
  (i : int{ 0 <= _ && _ <= k }) ->
  (j : int{ k <= _ && _ <= n }) ->
  (t : slice{ fill_inv (now m) (now _) k i j }) @ local unique ->
  (slice * slice){
    now (fst _) = now m && fin (fst _) = fin m
    && now (snd _) = merged (take k (now m)) (drop k (now m))
    && fin (snd _) = fin t
  } @ local unique =
  fun k m n i j t ->
  exclave_
    (if i = k && j = n
     then (m, t)
     else if j = n
     then begin
       let (v, m1) = sget m i in
       let t1 = sset t (i + j - k) v in
       let r = fill_loop k m1 n (i + 1) j t1 in
       r
     end
     else if i = k
     then begin
       let (v, m1) = sget m j in
       let t1 = sset t (i + j - k) v in
       let r = fill_loop k m1 n i (j + 1) t1 in
       r
     end
     else begin
       let (vi, m1) = sget m i in
       let (vj, m2) = sget m1 j in
       if vi <= vj
       then begin
         let t1 = sset t (i + j - k) vi in
         let r = fill_loop k m2 n (i + 1) j t1 in
         r
       end
       else begin
         let t1 = sset t (i + j - k) vj in
         let r = fill_loop k m2 n i (j + 1) t1 in
         r
       end
     end)

(* Copy-back phase: positions [0, idx) of the destination already
   agree with the source; each step copies one more element. *)
let rec copy_loop :
  (t : slice) @ local unique ->
  (m : slice{ len (now _) = len (now t) }) @ local unique ->
  (idx : int{ 0 <= _ && _ <= len (now t)
              && take _ (now m) = take _ (now t) }) ->
  (slice * slice){
    now (fst _) = now t && fin (fst _) = fin t
    && now (snd _) = now t && fin (snd _) = fin m
  } @ local unique =
  fun t m idx ->
  exclave_
    (let (n, t1) = slen t in
     if idx = n
     then (t1, m)
     else begin
       let (v, t2) = sget t1 idx in
       let m1 = sset m idx v in
       let r = copy_loop t2 m1 (idx + 1) in
       r
     end)

let merge_sorted_halves :
  (k : int{ 0 <= _ }) ->
  (m : slice{ k <= len (now _) }) @ local unique ->
  unit{ fin m = merged (take k (now m)) (drop k (now m)) } =
  fun k m ->
  let (n, m0) = slen m in
  let temp = anew n 0 in
  let tp = new_proph () in
  let (trest, proof) =
    borrow tp temp (fun t ->
      let filled = fill_loop k m0 n 0 k t in
      let (m1, t1) = filled in
      let copied = copy_loop t1 m1 0 in
      let (_t2, m2) = copied in
      let d = sdrop m2 in
      (d : unit{ fin m = merged (take k (now m)) (drop k (now m)) }))
  in
  ignore trest;
  proof
