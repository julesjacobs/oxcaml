open Vslice
open Vslice_model

let swap (loan : slice @ local unique)
    (first : int{ 0 <= _ && _ < len (current loan) })
    (second : int{ 0 <= _ && _ < len (current loan) })
    : slice{
        current _ =
          update
            (update (current loan) first (elem (current loan) second))
            second (elem (current loan) first)
        && perm (current loan) (current _) = true
        && final _ = final loan
      } @ local unique =
  exclave_
    (let values_snapshot, loan = snapshot ~loan in
     let values = take_snapshot_values ~snapshot:values_snapshot in
     let first_value, loan = slice_get ~loan ~index:first in
     let second_value, loan = slice_get ~loan ~index:second in
     len_update_law ~values ~index:first
       ~value:second_value;
     perm_swap_law ~values ~first ~second;
     let loan = slice_set ~loan ~index:first ~value:second_value in
     slice_set ~loan ~index:second ~value:first_value)

let rec partition (pivot : int) (lower : int{ 0 <= _ })
    (scan : int{ lower <= _ })
    (original : int list @ logical)
    (frame : final_frame @ local)
    (loan :
       slice{
         scan <= len (current _) - 1
         && elem (current _) (len (current _) - 1) = pivot
         && all_le (take lower (current _)) pivot = true
         && all_ge (segment lower scan (current _)) pivot = true
         && perm original (current _) = true
         && final _ = final_frame_values frame
       } @ local unique)
    (size : int{ _ = len (current loan) })
    (local_ continue :
       (index:int{ 0 <= _ && _ < size } ->
        result_loan:
          slice{
            len (current _) = size
            && elem (current _) index = pivot
            && all_le (take index (current _)) pivot = true
            && all_ge (drop (index + 1) (current _)) pivot = true
            && perm original (current _) = true
            && final _ = final_frame_values frame
          } @ local unique ->
        'a @ local unique) @ once)
    : 'a @ local unique =
  exclave_
    (if scan < size - 1
     then begin
       let before, loan = snapshot ~loan in
       let values = take_snapshot_values ~snapshot:before in
       let value, loan = slice_get ~loan ~index:scan in
       if value <= pivot
       then begin
         let first_update = update values lower (elem values scan) in
         swap_le_law ~values ~pivot ~lower ~scan
           ~prefix:() ~scanned:();
         swap_mid_law ~values ~pivot ~lower ~scan ~middle:();
         len_update_law
           ~values ~index:lower ~value:(elem values scan);
         len_update_law
           ~values:first_update ~index:scan ~value:(elem values lower);
         elem_update_law
           ~values ~update_index:lower ~query:(size - 1)
           ~value:(elem values scan);
         elem_update_law
           ~values:first_update ~update_index:scan ~query:(size - 1)
           ~value:(elem values lower);
         perm_swap_law ~values ~first:lower ~second:scan;
         perm_trans_law
           ~first:original ~second:values
           ~third:
             (update
                (update values lower (elem values scan))
                scan (elem values lower))
           ~left:() ~right:();
         let loan = swap loan lower scan in
         partition pivot (lower + 1) (scan + 1)
           original frame loan size continue
       end
       else begin
         segment_grow_ge_law
           ~values ~pivot ~first:lower ~last:scan
           ~middle:() ~next:();
         partition pivot lower (scan + 1)
           original frame loan size continue
       end
     end
     else begin
       let before, loan = snapshot ~loan in
       let values = take_snapshot_values ~snapshot:before in
       let first_update = update values lower (elem values scan) in
       len_update_law
         ~values ~index:lower ~value:(elem values scan);
       len_update_law
         ~values:first_update ~index:scan ~value:(elem values lower);
       take_update_ge_law
         ~count:lower ~index:lower ~value:(elem values scan)
         ~values ~proof:();
       take_update_ge_law
         ~count:lower ~index:scan ~value:(elem values lower)
         ~values:first_update ~proof:();
       swap_pivot_law ~values ~pivot ~lower ~scan ~parked:();
       swap_final_ge_law
         ~values ~pivot ~lower ~scan ~final_index:() ~middle:();
       perm_swap_law ~values ~first:lower ~second:scan;
       perm_trans_law
         ~first:original ~second:values
         ~third:
           (update
              (update values lower (elem values scan))
              scan (elem values lower))
         ~left:() ~right:();
       let loan = swap loan lower scan in
       continue ~index:lower ~result_loan:loan
     end)

let rec qsort (loan : slice @ local unique)
    : snapshot{
      snapshot_values _ = final loan
        && sorted (snapshot_values _) = true
        && perm (current loan) (snapshot_values _) = true
      } @ local unique =
  exclave_
   (let size, loan = slice_length ~loan in
    if size <= 1
  then begin
    let before, loan = snapshot ~loan in
    let values = take_snapshot_values ~snapshot:before in
    sorted_short_law ~values ~proof:();
    perm_refl_law ~values;
    let result, loan = snapshot ~loan in
    close ~loan;
    result
  end
  else begin
    let root, loan = snapshot ~loan in
    let original = take_snapshot_values ~snapshot:root in
    let frame, loan = capture_final_frame ~loan in
    let pivot, loan = slice_get ~loan ~index:(size - 1) in
    perm_refl_law ~values:original;
    take_nonpositive_law ~count:0 ~values:original ~proof:();
    segment_empty_law ~values:original ~first:0;
    empty_bounds_law ~bound:pivot;
    partition pivot 0 0 original frame loan size
      (fun ~index ~result_loan ->
         exclave_
          (let partition_snapshot, result_loan =
           snapshot ~loan:result_loan
         in
         let values =
           take_snapshot_values ~snapshot:partition_snapshot
         in
         segment_one_law ~values ~index;
         decomposition_perm_law
           ~values ~first:index ~last:(index + 1);
         perm_trans_law
           ~first:original ~second:values
           ~third:
             (append
                (take index values)
                (append
                   (segment index (index + 1) values)
                   (drop (index + 1) values)))
           ~left:() ~right:();
         let first_prophecy = new_prophecy () in
         let middle_prophecy = new_prophecy () in
         let last_prophecy = new_prophecy () in
         let result_loan, proof =
           split3
             ~first_prophecy ~middle_prophecy ~last_prophecy
             ~loan:result_loan ~first:index ~last:(index + 1)
             (fun ~first_loan ~middle_loan ~last_loan ->
                let first_snapshot = qsort first_loan in
                let middle_snapshot, middle_loan =
                  snapshot ~loan:middle_loan
                in
                close ~loan:middle_loan;
                let last_snapshot = qsort last_loan in
                let first_result =
                  take_snapshot_values ~snapshot:first_snapshot
                in
                let middle_result =
                  take_snapshot_values ~snapshot:middle_snapshot
                in
                let last_result =
                  take_snapshot_values ~snapshot:last_snapshot
                in
                all_le_perm_law
                  ~source:(take index values) ~target:first_result
                  ~bound:pivot ~permutation:() ~source_bound:();
                all_ge_perm_law
                  ~source:(drop (index + 1) values)
                  ~target:last_result ~bound:pivot
                  ~permutation:() ~source_bound:();
                sorted_pivot_glue_law
                  ~left:first_result ~right:last_result ~pivot
                  ~left_sorted:() ~right_sorted:()
                  ~left_bound:() ~right_bound:();
                perm_refl_law
                  ~values:(segment index (index + 1) values);
                perm_glue3_law
                  ~original
                  ~left:(take index values)
                  ~middle:(segment index (index + 1) values)
                  ~right:(drop (index + 1) values)
                  ~left_result:first_result
                  ~middle_result ~right_result:last_result
                  ~decomposed:() ~left_perm:()
                  ~middle_perm:() ~right_perm:();
                (() : unit{
                   sorted
                     (append
                        (prophecy_value first_prophecy)
                        (append
                           (prophecy_value middle_prophecy)
                           (prophecy_value last_prophecy)))
                   = true
                   && perm
                        original
                        (append
                           (prophecy_value first_prophecy)
                           (append
                              (prophecy_value middle_prophecy)
                              (prophecy_value last_prophecy)))
                      = true
                 }))
         in
         let () = proof in
         let final_snapshot, result_loan = snapshot ~loan:result_loan in
         close ~loan:result_loan;
         (final_snapshot : snapshot{
            snapshot_values _ = final_frame_values frame
            && sorted (snapshot_values _) = true
            && perm original (snapshot_values _) = true
          } @ local)))
    end)

let rec psort (loan : slice @ local unique)
    : snapshot{
      snapshot_values _ = final loan
        && sorted (snapshot_values _) = true
        && perm (current loan) (snapshot_values _) = true
      } @ local unique =
  exclave_
   (let size, loan = slice_length ~loan in
    if size <= 1
    then begin
      let before, loan = snapshot ~loan in
      let values = take_snapshot_values ~snapshot:before in
      sorted_short_law ~values ~proof:();
      perm_refl_law ~values;
      let result, loan = snapshot ~loan in
      close ~loan;
      result
    end
    else begin
      let root, loan = snapshot ~loan in
      let original = take_snapshot_values ~snapshot:root in
      let frame, loan = capture_final_frame ~loan in
      let pivot, loan = slice_get ~loan ~index:(size - 1) in
      perm_refl_law ~values:original;
      take_nonpositive_law ~count:0 ~values:original ~proof:();
      segment_empty_law ~values:original ~first:0;
      empty_bounds_law ~bound:pivot;
      partition pivot 0 0 original frame loan size
        (fun ~index ~result_loan ->
           exclave_
            (let partition_snapshot, result_loan =
               snapshot ~loan:result_loan
             in
             let values =
               take_snapshot_values ~snapshot:partition_snapshot
             in
             segment_one_law ~values ~index;
             decomposition_perm_law
               ~values ~first:index ~last:(index + 1);
             perm_trans_law
               ~first:original ~second:values
               ~third:
                 (append
                    (take index values)
                    (append
                       (segment index (index + 1) values)
                       (drop (index + 1) values)))
               ~left:() ~right:();
             let first_prophecy = new_prophecy () in
             let middle_prophecy = new_prophecy () in
             let last_prophecy = new_prophecy () in
             let result_loan, proof =
               split3
                 ~first_prophecy ~middle_prophecy ~last_prophecy
                 ~loan:result_loan ~first:index ~last:(index + 1)
                 (fun ~first_loan ~middle_loan ~last_loan ->
                    let middle_snapshot, middle_loan =
                      snapshot ~loan:middle_loan
                    in
                    close ~loan:middle_loan;
                    let first_snapshot, last_snapshot =
                      Fork_join.fork_join2
                        (fun () ->
                           let local_snapshot = psort first_loan in
                           let global_values =
                             take_snapshot_values
                               ~snapshot:local_snapshot
                           in
                           (global_values : int list{
                              _ =
                                prophecy_value first_prophecy
                              && sorted _ = true
                              && perm
                                   (take index values)
                                   _
                                 = true
                            } @ unique))
                        (fun () ->
                           let local_snapshot = psort last_loan in
                           let global_values =
                             take_snapshot_values
                               ~snapshot:local_snapshot
                           in
                           (global_values : int list{
                              _ =
                                prophecy_value last_prophecy
                              && sorted _ = true
                              && perm
                                   (drop (index + 1) values)
                                   _
                                 = true
                            } @ unique))
                    in
                    let first_result = first_snapshot in
                    let middle_result =
                      take_snapshot_values ~snapshot:middle_snapshot
                    in
                    let last_result = last_snapshot in
                    all_le_perm_law
                      ~source:(take index values) ~target:first_result
                      ~bound:pivot ~permutation:() ~source_bound:();
                    all_ge_perm_law
                      ~source:(drop (index + 1) values)
                      ~target:last_result ~bound:pivot
                      ~permutation:() ~source_bound:();
                    sorted_pivot_glue_law
                      ~left:first_result ~right:last_result ~pivot
                      ~left_sorted:() ~right_sorted:()
                      ~left_bound:() ~right_bound:();
                    perm_refl_law
                      ~values:(segment index (index + 1) values);
                    perm_glue3_law
                      ~original
                      ~left:(take index values)
                      ~middle:(segment index (index + 1) values)
                      ~right:(drop (index + 1) values)
                      ~left_result:first_result
                      ~middle_result ~right_result:last_result
                      ~decomposed:() ~left_perm:()
                      ~middle_perm:() ~right_perm:();
                    (() : unit{
                       sorted
                         (append
                            (prophecy_value first_prophecy)
                            (append
                               (prophecy_value middle_prophecy)
                               (prophecy_value last_prophecy)))
                       = true
                       && perm
                            original
                            (append
                               (prophecy_value first_prophecy)
                               (append
                                  (prophecy_value middle_prophecy)
                                  (prophecy_value last_prophecy)))
                          = true
                     }))
             in
             let () = proof in
             let final_snapshot, result_loan = snapshot ~loan:result_loan in
             close ~loan:result_loan;
             (final_snapshot : snapshot{
                snapshot_values _ = final_frame_values frame
                && sorted (snapshot_values _) = true
                && perm original (snapshot_values _) = true
              } @ local)))
    end)

let sort_array ~(array : array @ unique)
    : array{
        sorted (contents _) = true
        && perm (contents array) (contents _) = true
      } @ unique =
  let prophecy = new_prophecy () in
  let array, proof =
    borrow ~prophecy ~array
      (fun ~loan ->
         let result = qsort loan in
         let _result_values = take_snapshot_values ~snapshot:result in
         (() : unit{
            sorted (prophecy_value prophecy) = true
            && perm (contents array) (prophecy_value prophecy) = true
          }))
  in
  let () = proof in
  array

let demo () =
  let array = make ~n:8 ~value:5 in
  sort_array ~array

let parallel_sort_array ~(array : array @ unique)
    : array{
        sorted (contents _) = true
        && perm (contents array) (contents _) = true
      } @ unique =
  let prophecy = new_prophecy () in
  let array, proof =
    borrow ~prophecy ~array
      (fun ~loan ->
         let result = psort loan in
         let _result_values = take_snapshot_values ~snapshot:result in
         (() : unit{
            sorted (prophecy_value prophecy) = true
            && perm (contents array) (prophecy_value prophecy) = true
          }))
  in
  let () = proof in
  array

let parallel_demo () =
  let array = make ~n:8 ~value:5 in
  parallel_sort_array ~array
