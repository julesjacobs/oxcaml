let unsupported_claim () =
  let prophecy = Vslice.new_prophecy () in
  let (_ : unit{ Vslice.prophecy_value prophecy = [] }) = () in
  ()
