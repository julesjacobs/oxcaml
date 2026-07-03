(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "slice_lib.mli slice_lib.ml par_lib.mli par_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* IN-PLACE QUICKSORT over RustHorn-style slice borrows (slice_lib),
   verified against [sorted]-and-[perm]: every obligation below is
   really proved through Lean's grind.  The recursion is a REBORROW:
   [split] hands a bracket two sub-loans with fresh prophecies, each
   side is sorted and RESOLVED ([sdrop]), and the facts come home as
   prophecy values -- exports are phrased in lender-scope terms, and
   the parent loan returns already advanced to the prophesied
   recombination.  No join, no adjacency side conditions: the
   bracket's scoping is the adjacency proof.  Then the SAME
   verification, parallel: [psort] hands the two sub-loans to
   [Par_lib.fork_join2] as once-closures -- disjointness is the mode
   checker's, and each task's result is its side's resolution facts.
   The negative probes (bounds, prophecy reuse, forged exports, loan
   escape, racing tasks) live in mechanics/lean_qsort_fail.ml. *)

open Slice_lib

(* Swap two in-bounds positions of a loan: four threaded ops; the
   composed [upd]/[elem] contents expression is re-proved at the
   annotation by congruence alone, and [fin] just threads. *)
let swap :
  (m : slice) @ local unique ->
  (i : int{ 0 <= _ && _ < len (now m) }) ->
  (j : int{ 0 <= _ && _ < len (now m) }) ->
  slice{ now _ = upd (upd (now m) i (elem (now m) j)) j (elem (now m) i)
         && fin _ = fin m }
    @ local unique =
  fun m i j ->
    exclave_
      (let (vi, m1) = sget m i in
       let (vj, m2) = sget m1 j in
       let m3 = sset m2 i vj in
       let m4 = sset m3 j vi in
       m4)

(* Lomuto partition with the pivot parked at the last position.  The
   invariant rides the parameters: positions [0,i) are <= p, the
   window [seg i j] is >= p, the pivot p sits at [n-1], and [j] scans
   until it reaches it.  Each recursive call discharges the halved
   invariant from the path facts and the swap lemmas
   ([swap_le]/[swap_mid]); the final swap parks the pivot at [i], and
   [swap_final_ge] turns the window into the >=-suffix. *)
let rec part :
  (p : int) -> (i : int{ 0 <= _ }) -> (j : int{ i <= _ }) ->
  (s : slice{ j <= len (now _) - 1
              && elem (now _) (len (now _) - 1) = p
              && all_le (take i (now _)) p
              && all_ge (seg i j (now _)) p }) @ local unique ->
  (n : int{ _ = len (now s) }) ->
  (int * slice){ 0 <= fst _ && fst _ < n
                 && elem (now (snd _)) (fst _) = p
                 && all_le (take (fst _) (now (snd _))) p
                 && all_ge (drop (fst _) (now (snd _))) p
                 && perm (now s) (now (snd _))
                 && fin (snd _) = fin s }
    @ local unique =
  fun p i j s n ->
    exclave_
      (if j < n - 1
       then begin
         let (v, s1) = sget s j in
         if v <= p
         then begin
           let s2 = swap s1 i j in
           let q = part p (i + 1) (j + 1) s2 n in
           q
         end
         else begin
           let q = part p i (j + 1) s1 n in
           q
         end
       end
       else begin
         let s' = swap s i j in
         ((i : int), s')
       end)

(* Sequential quicksort on a loan: read the pivot, partition, then
   REBORROW -- the outer split at the pivot index, the inner split
   isolating the pivot's singleton.  Each side is sorted recursively
   and resolved; the singleton is resolved as-is; resolving the inner
   bracket's advanced parent reveals [pv prest] as pivot-then-right.
   Each bracket's export restates its prophecies' facts in the scope
   OUTSIDE the bracket, and the parent loan comes back at
   [app (pv pl) (pv prest)], where the glue lemmas finish. *)
let rec qsort :
  (m : slice) @ local unique ->
  slice{ perm (now m) (now _) && sorted (now _) && fin _ = fin m }
    @ local unique =
  fun m ->
    exclave_
      (let (n, m0) = slen m in
       if n <= 1
       then m0
       else begin
         let (p, mp) = sget m0 (n - 1) in
         let q = part p 0 0 mp n in
         let (k, m2) = q in
         let pl = new_proph () in
         let prest = new_proph () in
         let (mres, u) =
           split pl prest m2 k (fun left rest ->
             let left' = qsort left in
             let _u1 = sdrop left' in
             let pm = new_proph () in
             let pr = new_proph () in
             let (rest_adv, u2) =
               split pm pr rest 1 (fun mid right ->
                 let right' = qsort right in
                 let _u2 = sdrop right' in
                 let _u3 = sdrop mid in
                 (() : unit{ pv pm = take 1 (now rest)
                             && sorted (pv pr)
                             && perm (drop 1 (now rest)) (pv pr) }))
             in
             ignore u2;
             let _u4 = sdrop rest_adv in
             (() : unit{ sorted (pv pl)
                         && perm (take k (now m2)) (pv pl)
                         && sorted (pv prest)
                         && perm (drop k (now m2)) (pv prest) }))
         in
         ignore u;
         mres
       end)

(* Parallel quicksort, same spec: inside the inner bracket both
   sub-loans are live and disjoint, so each once-closure consumes one
   and fork_join2 runs them on separate domains -- each task returns
   its side's resolution facts (global refined units), which survive
   the join; everything else is the sequential proof. *)
let rec psort :
  (m : slice) @ local unique ->
  slice{ perm (now m) (now _) && sorted (now _) && fin _ = fin m }
    @ local unique =
  fun m ->
    exclave_
      (let (n, m0) = slen m in
       if n <= 1
       then m0
       else begin
         let (p, mp) = sget m0 (n - 1) in
         let q = part p 0 0 mp n in
         let (k, m2) = q in
         let pl = new_proph () in
         let prest = new_proph () in
         let (mres, u) =
           split pl prest m2 k (fun left rest ->
             let pm = new_proph () in
             let pr = new_proph () in
             let (rest_adv, u2) =
               split pm pr rest 1 (fun mid right ->
                 let (ul, ur) =
                   Par_lib.fork_join2
                     (fun () ->
                       let left' = psort left in
                       let _u = sdrop left' in
                       (() : unit{ sorted (pv pl)
                                   && perm (take k (now m2)) (pv pl) }))
                     (fun () ->
                       let right' = psort right in
                       let _u = sdrop right' in
                       (() : unit{ sorted (pv pr)
                                   && perm (drop 1 (now rest)) (pv pr) }))
                 in
                 ignore ul;
                 ignore ur;
                 let _u3 = sdrop mid in
                 (() : unit{ pv pm = take 1 (now rest)
                             && sorted (pv pr)
                             && perm (drop 1 (now rest)) (pv pr)
                             && sorted (pv pl)
                             && perm (take k (now m2)) (pv pl) }))
             in
             ignore u2;
             let _u4 = sdrop rest_adv in
             (() : unit{ sorted (pv pl)
                         && perm (take k (now m2)) (pv pl)
                         && sorted (pv prest)
                         && perm (drop k (now m2)) (pv prest) }))
         in
         ignore u;
         mres
       end)

(* Client-side payoff: borrow the array, sort the root loan in
   parallel, resolve -- the residual array provably holds the sorted
   permutation, and any two positions read back in order
   ([sorted_elem] bridges the chain invariant to the probed
   indices). *)
let probe :
  (x : varr) @ unique ->
  (i : int{ 0 <= _ }) ->
  (j : int{ i <= _ && _ < len (cts x) }) ->
  (int * int){ fst _ <= snd _ } =
  fun x i j ->
    let p = new_proph () in
    let (x', u) =
      borrow p x (fun m ->
        let ms = psort m in
        let _u = sdrop ms in
        (() : unit{ sorted (pv p) && perm (cts x) (pv p) }))
    in
    ignore u;
    let (vi, x1) = aget x' i in
    let (vj, x2) = aget x1 j in
    ignore x2;
    ((vi, vj) : (int * int){ fst _ <= snd _ })

(* A concrete run end to end: fill, scramble through a first borrow,
   sort-and-probe through a second -- no obligations left at the
   caller. *)
let run : unit -> (int * int){ fst _ <= snd _ } =
  fun () ->
    let x = anew 8 5 in
    let p0 = new_proph () in
    let (x1, u0) =
      borrow p0 x (fun m ->
        let m1 = sset m 0 3 in
        let m2 = sset m1 3 9 in
        let m3 = sset m2 7 1 in
        let _u = sdrop m3 in
        (() : unit{ len (pv p0) = len (cts x) }))
    in
    ignore u0;
    let q = probe x1 0 7 in
    q
