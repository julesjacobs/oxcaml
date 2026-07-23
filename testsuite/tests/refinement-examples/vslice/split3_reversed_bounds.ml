let test () =
  let array = Vslice.make ~n:2 ~value:0 in
  let prophecy = Vslice.new_prophecy () in
  Vslice.borrow ~prophecy ~array (fun ~loan ->
    let first_prophecy = Vslice.new_prophecy () in
    let middle_prophecy = Vslice.new_prophecy () in
    let last_prophecy = Vslice.new_prophecy () in
    let _loan, () =
      Vslice.split3
        ~first_prophecy ~middle_prophecy ~last_prophecy
        ~loan ~first:1 ~last:0
        (fun ~first_loan:_ ~middle_loan:_ ~last_loan:_ -> ())
    in
    ())
