(* TEST
 arch_amd64;
 llvm-backend;
 flags += " -O3 -llvm-backend";
 native;
*)

let check label ok = if not ok then failwith label

let hits = ref 0

let keep = ref (Some (String.make 1000 'x'))

let () =
  check "enabled probe_is_enabled" [%probe_is_enabled "enabled"];
  [%probe
    "enabled" ~enabled_at_init:true
      (incr hits;
       Gc.full_major ())];
  check "enabled probe ran" (!hits = 1);
  check "root live across probe"
    (match !keep with Some s -> String.length s = 1000 | None -> false);
  check "disabled probe_is_enabled" (not [%probe_is_enabled "disabled"]);
  [%probe "disabled" ~enabled_at_init:false (incr hits)];
  check "disabled probe did not run" (!hits = 1);
  let raised =
    try
      [%probe "raises" ~enabled_at_init:true (failwith "probe")];
      false
    with Failure _ -> true
  in
  check "probe exception" raised
