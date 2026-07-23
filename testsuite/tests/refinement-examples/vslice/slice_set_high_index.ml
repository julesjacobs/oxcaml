let test () =
  let array = Vslice.make ~n:2 ~value:0 in
  let prophecy = Vslice.new_prophecy () in
  Vslice.borrow ~prophecy ~array (fun ~loan ->
    let _loan = Vslice.slice_set ~loan ~index:2 ~value:1 in
    ())
