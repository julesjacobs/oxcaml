(* Real arithmetic is ON by default (the LRA rollout flip). Tri-state so the flip is
   reversible for A/B and byte-recovery, following the fleet-wide env convention
   (DESIGN-LRA.md 2.1): an UNRECOGNIZED value resolves to the DEFAULT direction (garbage
   is treated as unset), and only the recognized opt-out tokens flip to the non-default
   side.
   - unset -> ON (the new default)
   - "0"/"false"/"no"/"off" (any case, surrounding whitespace trimmed) -> OFF; this
     force-OFF byte-recovers trunk exactly
   - any other value ("1"/"true"/... and any typo) -> ON (the default). Read at most once.
     Every gate routes through {!enabled}; there is no bare is-set check anywhere (which
     would misread the force-OFF value as ON). *)
let value =
  lazy
    (match Sys.getenv_opt "OXSMT_LRA" with
     | None -> true
     | Some value ->
       (* Trim before matching so an opt-out picked up from a script (a trailing space or
          newline, [OXSMT_LRA="off\n"]) still selects OFF instead of falling through to the
          default; a genuinely unrecognized value still resolves to the default per the
          convention above. *)
       (match String.trim (String.lowercase_ascii value) with
        | "0" | "false" | "no" | "off" -> false
        | _ -> true))
;;

let enabled () = Lazy.force value
