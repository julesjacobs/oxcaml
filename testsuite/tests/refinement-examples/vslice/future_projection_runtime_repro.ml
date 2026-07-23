(* The specification-only boundary rejects the call to [final_frame_values]
   in this value-level expression before it can observe a future value. *)
let () =
  let array = Vslice.make ~n:2 ~value:7 in
  let prophecy = Vslice.new_prophecy () in
  let _array, () =
    Vslice.borrow ~prophecy ~array (fun ~loan ->
      let frame, loan = Vslice.capture_final_frame ~loan in
      let predicted = Vslice.final_frame_values frame in
      let loan = Vslice.slice_set ~loan ~index:0 ~value:9 in
      let snapshot, loan = Vslice.snapshot ~loan in
      let current = Vslice.take_snapshot_values ~snapshot in
      let () = Vslice.close ~loan in
      let (_ : unit{ predicted = current }) = () in
      assert (predicted = current))
  in
  ()
