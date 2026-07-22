#syntax quotations on

let double_cancellation_checks_current_stage () =
  (<[ <[
        $($(ignore (0 : int{ false }); <[ <[ 0 ]> ]>))
      ]> ]>
   [@magic_staged_modes])
