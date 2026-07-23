let test () =
  let array = Vslice.make ~n:2 ~value:0 in
  let _value, _array = Vslice.get ~array ~index:(-1) in
  ()
