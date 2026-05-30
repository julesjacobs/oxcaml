let loops = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1
let bunch = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 1
let _ = loops + bunch
let () = print_endline "pass"
