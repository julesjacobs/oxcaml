let test () =
  let array = Vslice.make ~n:2 ~value:0 in
  let prophecy = Vslice.new_prophecy () in
  Vslice.borrow ~prophecy ~array (fun ~loan ->
    let _value, _loan = Vslice.slice_get ~loan ~index:2 in
    ())
