(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "slice_lib.mli slice_lib.ml par_lib.mli par_lib.ml slice_sort_lib.mli slice_sort_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* MERGE SORT over slice borrows, fully verified: the merge itself is
   the verified loop of slice_sort_lib (no assumptions anywhere but
   slice_lib's own signatures).  Where quicksort threads live loans
   back out, merge sort is written PROOF-STYLE: sorting a loan
   CONSUMES and RESOLVES it, and the facts come home as a refined
   unit over [fin] -- the same shape as [sdrop], composed through the
   split brackets.  The halves' sortedness at the merge site is
   recovered from the unwidened [split] spec alone: [perm_len] pins
   the prophecies' lengths, and take_app_exact/drop_app_exact cut the
   recombination back into the sorted halves. *)

open Slice_lib
open Slice_sort_lib

let merge :
  (k : int{ 0 <= _ }) ->
  (m : slice{
     k <= len (now _)
     && sorted (take k (now _))
     && sorted (drop k (now _))
   }) @ local unique ->
  unit{ sorted (fin m) && perm (now m) (fin m) } =
  fun k m ->
  let _spec = merge_sorted_halves k m in
  (() : unit{ sorted (fin m) && perm (now m) (fin m) })

let rec sort_slice :
  (m : slice) @ local unique ->
  unit{ sorted (fin m) && perm (now m) (fin m) } =
  fun m ->
  let (n, m0) = slen m in
  if n <= 1
  then begin
    let _done = sdrop m0 in
    (() : unit{ sorted (fin m) && perm (now m) (fin m) })
  end
  else begin
    let middle = n / 2 in
    let left_final = new_proph () in
    let right_final = new_proph () in
    let (m1, proof) =
      split left_final right_final m0 middle (fun left right ->
        let left_done = sort_slice left in
        let right_done = sort_slice right in
        ((left_done, right_done)
          : unit{
              sorted (pv left_final)
              && perm (take middle (now m0)) (pv left_final)
            }
            * unit{
                sorted (pv right_final)
                && perm (drop middle (now m0)) (pv right_final)
              }))
    in
    let (_left_done, _right_done) = proof in
    let _combined =
      (() : unit{ perm (now m0) (now m1) })
    in
    let _merged = merge middle m1 in
    (() : unit{ sorted (fin m) && perm (now m) (fin m) })
  end

let rec parallel_sort_slice :
  (m : slice) @ local unique ->
  unit{ sorted (fin m) && perm (now m) (fin m) } =
  fun m ->
  let (n, m0) = slen m in
  if n <= 1
  then begin
    let _done = sdrop m0 in
    (() : unit{ sorted (fin m) && perm (now m) (fin m) })
  end
  else begin
    let middle = n / 2 in
    let left_final = new_proph () in
    let right_final = new_proph () in
    let (m1, proof) =
      split left_final right_final m0 middle (fun left right ->
        let (left_done, right_done) =
          Par_lib.fork_join2
            (fun () ->
              let done_ = parallel_sort_slice left in
              (done_
                : unit{
                    sorted (pv left_final)
                    && perm (take middle (now m0)) (pv left_final)
                  }))
            (fun () ->
              let done_ = parallel_sort_slice right in
              (done_
                : unit{
                    sorted (pv right_final)
                    && perm (drop middle (now m0)) (pv right_final)
                  }))
        in
        ((left_done, right_done)
          : unit{
              sorted (pv left_final)
              && perm (take middle (now m0)) (pv left_final)
            }
            * unit{
                sorted (pv right_final)
                && perm (drop middle (now m0)) (pv right_final)
              }))
    in
    let (_left_done, _right_done) = proof in
    let _combined =
      (() : unit{ perm (now m0) (now m1) })
    in
    let _merged = merge middle m1 in
    (() : unit{ sorted (fin m) && perm (now m) (fin m) })
  end

let sort :
  (x : varr) @ unique ->
  varr{ sorted (cts _) && perm (cts x) (cts _) } @ unique =
  fun x ->
  let final = new_proph () in
  let (x', proof) =
    borrow final x (fun m ->
      let done_ = sort_slice m in
      (done_ : unit{ sorted (pv final) && perm (cts x) (pv final) }))
  in
  ignore proof;
  x'

let parallel_sort :
  (x : varr) @ unique ->
  varr{ sorted (cts _) && perm (cts x) (cts _) } @ unique =
  fun x ->
  let final = new_proph () in
  let (x', proof) =
    borrow final x (fun m ->
      let done_ = parallel_sort_slice m in
      (done_ : unit{ sorted (pv final) && perm (cts x) (pv final) }))
  in
  ignore proof;
  x'

let probe :
  (x : varr) @ unique ->
  (i : int{ 0 <= _ }) ->
  (j : int{ i <= _ && _ < len (cts x) }) ->
  (int * int){ fst _ <= snd _ } =
  fun x i j ->
  let sorted = parallel_sort x in
  let (left, sorted) = aget sorted i in
  let (right, sorted) = aget sorted j in
  ignore sorted;
  (left, right)

let run : unit -> (int * int){ fst _ <= snd _ } =
  fun () ->
  let x = anew 8 5 in
  let prophecy = new_proph () in
  let (x, proof) =
    borrow prophecy x (fun m ->
      let m = sset m 0 3 in
      let m = sset m 3 9 in
      let m = sset m 7 1 in
      let done_ = sdrop m in
      (done_ : unit{ len (pv prophecy) = len (cts x) }))
  in
  ignore proof;
  probe x 0 7
