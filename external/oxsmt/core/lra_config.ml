(* Real arithmetic is ON by default (the LRA rollout flip). Tri-state so the flip is
   reversible for A/B and byte-recovery:
   - unset -> ON (the new default)
   - "0"/"false"/"no"/"off" (any case) -> OFF; this force-OFF byte-recovers trunk exactly
   - any other value ("1"/"true"/...) -> ON Read at most once. Every gate routes through
     {!enabled}; there is no bare is-set check anywhere (which would misread the force-OFF
     value as ON). *)
let value =
  lazy
    (match Sys.getenv_opt "OXSMT_LRA" with
     | None -> true
     | Some value ->
       (match String.lowercase_ascii value with
        | "0" | "false" | "no" | "off" -> false
        | _ -> true))
;;

let enabled () = Lazy.force value
