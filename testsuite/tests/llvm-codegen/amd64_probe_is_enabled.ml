(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 flags += " -O3 -llvm-backend -probes -no-probes-optimized";
 native;
*)

let hits = ref []

let () =
  [%probe "disabled" (hits := "disabled" :: !hits)];
  [%probe "enabled" ~enabled_at_init:true (hits := "enabled" :: !hits)];
  assert ([%probe_is_enabled "enabled"]);
  assert (not [%probe_is_enabled "disabled"]);
  assert (!hits = ["enabled"])
