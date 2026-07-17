let value =
  lazy
    (match Sys.getenv_opt "OXSMT_LRA" with
     | Some value ->
       (match String.lowercase_ascii value with
        | "1" | "true" | "yes" | "on" -> true
        | _ -> false)
     | None -> false)
;;

let enabled () = Lazy.force value
