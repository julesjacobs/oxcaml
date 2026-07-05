(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "slice_lib.mli slice_lib.ml par_lib.mli par_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* IN-PLACE QUICKSORT over RustHorn-style slice borrows (slice_lib),
   verified against [sorted]-and-[perm], with the PROOF-STYLE
   interface: sorting a loan CONSUMES and RESOLVES it, and the
   conclusion comes back as a refined unit over the prophesied final
   contents -- [unit{ sorted (fin m) && perm (now m) (fin m) }].  The
   recursion is a REBORROW: [split3] hands a bracket three sub-loans
   with fresh prophecies, and each recursive call's conclusion is
   already in prophecy terms (the sub-loan's [fin] IS its prophecy),
   so the bracket's export restates them with nothing to thread and
   nothing to drop but the pivot singleton.  Then the SAME
   verification, parallel: [psort] hands the two outer sub-loans to
   [Par_lib.fork_join2] as once-closures -- disjointness is the mode
   checker's, and each task's result is its side's conclusion.  The
   negative probes (bounds, prophecy reuse, forged exports, loan
   escape, racing tasks) live in mechanics/lean_qsort_fail.ml. *)

open Slice_lib

(* Swap two in-bounds positions of a loan: four threaded ops; the
   composed [upd]/[elem] contents expression is re-proved at the
   annotation by congruence alone, and [fin] just threads. *)
let swap (m : slice @ local unique)
    (i : int{ 0 <= _ && _ < len (now m) })
    (j : int{ 0 <= _ && _ < len (now m) })
  : slice{ now _ = upd (upd (now m) i (elem (now m) j)) j (elem (now m) i)
           && fin _ = fin m } @ local unique =
  exclave_
    (let (vi, m1) = sget m i in
     let (vj, m2) = sget m1 j in
     let m3 = sset m2 i vj in
     sset m3 j vi)

(* Lomuto partition with the pivot parked at the last position.  The
   invariant rides the parameters: positions [0,i) are <= p, the
   window [seg i j] is >= p, the pivot p sits at [n-1], and [j] scans
   until it reaches it.  Each recursive call discharges the halved
   invariant from the path facts and the swap lemmas
   ([swap_le]/[swap_mid]); the final swap parks the pivot at [i], and
   [swap_final_ge] turns the window into the >=-suffix. *)
let rec part (p : int) (i : int{ 0 <= _ }) (j : int{ i <= _ })
    (s : slice{ j <= len (now _) - 1
                && elem (now _) (len (now _) - 1) = p
                && all_le (take i (now _)) p
                && all_ge (seg i j (now _)) p } @ local unique)
    (n : int{ _ = len (now s) })
  : (int * slice){ 0 <= fst _ && fst _ < n
                   && elem (now (snd _)) (fst _) = p
                   && all_le (take (fst _) (now (snd _))) p
                   && all_ge (drop (fst _ + 1) (now (snd _))) p
                   && perm (now s) (now (snd _))
                   && fin (snd _) = fin s } @ local unique =
  exclave_
    (if j < n - 1
     then begin
       let (v, s1) = sget s j in
       if v <= p
       then begin
         let s2 = swap s1 i j in
         part p (i + 1) (j + 1) s2 n
       end
       else part p i (j + 1) s1 n
     end
     else begin
       let s' = swap s i j in
       ((i : int), s')
     end)

(* Sequential quicksort, proof-style: read the pivot, partition,
   then REBORROW -- one three-way split at the pivot's singleton.
   Each side is sorted-and-resolved by the recursive call itself; the
   singleton is resolved as-is, pinning its prophecy to the pivot by
   equality.  The parent loan comes back advanced to the prophesied
   recombination, and resolving it turns the glue into the
   conclusion. *)
let rec qsort (m : slice @ local unique)
  : unit{ sorted (fin m) && perm (now m) (fin m) } =
  let (n, m0) = slen m in
  if n <= 1
  then begin
    let _u = sdrop m0 in
    ()
  end
  else begin
    let (p, mp) = sget m0 (n - 1) in
    let (k, m2) = part p 0 0 mp n in
    let (p1, p2, p3) = (new_proph (), new_proph (), new_proph ()) in
    let (mres, _u) =
      split3 p1 p2 p3 m2 k (k + 1) (fun a b c ->
        let _u1 = qsort a in
        let _u2 = sdrop b in
        let _u3 = qsort c in
        (() : unit{ sorted (pv p1)
                    && perm (take k (now m2)) (pv p1)
                    && pv p2 = seg k (k + 1) (now m2)
                    && sorted (pv p3)
                    && perm (drop (k + 1) (now m2)) (pv p3) }))
    in
    let _uf = sdrop mres in
    ()
  end

(* Parallel quicksort, same spec: the bracket's two outer sub-loans
   are disjoint, so each once-closure consumes one and fork_join2
   runs them on separate domains -- each task's result is its side's
   conclusion, which survives the join. *)
let rec psort (m : slice @ local unique)
  : unit{ sorted (fin m) && perm (now m) (fin m) } =
  let (n, m0) = slen m in
  if n <= 1
  then begin
    let _u = sdrop m0 in
    ()
  end
  else begin
    let (p, mp) = sget m0 (n - 1) in
    let (k, m2) = part p 0 0 mp n in
    let (p1, p2, p3) = (new_proph (), new_proph (), new_proph ()) in
    let (mres, _u) =
      split3 p1 p2 p3 m2 k (k + 1) (fun a b c ->
        let (_ua, _uc) =
          Par_lib.fork_join2
            (fun () ->
              let u = psort a in
              (u : unit{ sorted (pv p1)
                         && perm (take k (now m2)) (pv p1) }))
            (fun () ->
              let u = psort c in
              (u : unit{ sorted (pv p3)
                         && perm (drop (k + 1) (now m2)) (pv p3) }))
        in
        let _ub = sdrop b in
        (() : unit{ sorted (pv p1)
                    && perm (take k (now m2)) (pv p1)
                    && pv p2 = seg k (k + 1) (now m2)
                    && sorted (pv p3)
                    && perm (drop (k + 1) (now m2)) (pv p3) }))
    in
    let _uf = sdrop mres in
    ()
  end

(* Sorting an ARRAY: borrow it, sort the loan, and the bracket hands
   the array back with the conclusion on its contents -- the callee's
   refined unit is the export, and the prophecy carries the facts
   across the borrow. *)
let sort_array (x : varr @ unique)
  : varr{ sorted (cts _) && perm (cts x) (cts _) } @ unique =
  let p = new_proph () in
  let (x', _u) =
    borrow p x (fun m ->
      let u = psort m in
      (u : unit{ sorted (pv p) && perm (cts x) (pv p) }))
  in
  x'

(* Client-side payoff: borrow the array and sort the root loan in
   parallel -- the callee's conclusion IS the export.  The residual
   array provably holds the sorted permutation, and any two positions
   read back in order ([sorted_elem] bridges the chain invariant to
   the probed indices). *)
let probe (x : varr @ unique)
    (i : int{ 0 <= _ })
    (j : int{ i <= _ && _ < len (cts x) })
  : (int * int){ fst _ <= snd _ } =
  let p = new_proph () in
  let (x', _u) =
    borrow p x (fun m ->
      let u = psort m in
      (u : unit{ sorted (pv p) && perm (cts x) (pv p) }))
  in
  let (vi, x1) = aget x' i in
  let (vj, _x2) = aget x1 j in
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
