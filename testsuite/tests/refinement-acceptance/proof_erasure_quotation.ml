#syntax quotations on

let retained_quotation_site () =
  <[
    let (erase_quoted @ total)
        (value : int @ logical)
        : unit{ value = value } =
      ()
    in
    erase_quoted 25
  ]>
;;
