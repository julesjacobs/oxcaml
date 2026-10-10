(* TEST
 flags = "-extension layouts_beta";
 { bytecode; }{ native; }
*)

type r = #{ live : int; proof : string @@ ghost }
let make n = #{ live = n; proof = (print_endline "construct"; "erased") }
let () =
  let r = make 41 in
  let r = #{ r with live = r.#live + 1 } in
  let r = #{ r with proof = (print_endline "update"; "erased") } in
  let #{ live; proof = _ } = r in
  assert (live = 42);
  let make () = print_endline "project"; r in
  let _ = (make ()).#proof in
  ()

type singleton = #{ proof : string @@ ghost }
type all = #{ a : int @@ ghost; b : string @@ ghost }
let () =
  let s = #{ proof = (print_endline "singleton"; "erased") } in
  let #{ proof = _ } = s in
  let a = #{ a = ghost_ 1; b = ghost_ "erased" } in
  let #{ a = _; b = _ } = a in
  print_endline "done"

type ('a : any) poly = #{ live : int; hidden : 'a @@ ghost }
let use_float (_ : float# @ ghost) n = n
let read_float (r : float# poly) =
  let #{ live; hidden } = r in use_float hidden live
let () = assert (read_float #{ live = 42; hidden = #1.0 } = 42)

(* A record of one ghost field inside a boxed and an unboxed record: its
   layout is void, not its field's. Both used to crash the compiler. *)
type holder = { hx : singleton; hy : int }
type uholder = #{ ux : singleton; uy : int }
let () =
  let h = { hx = #{ proof = ghost_ "erased" }; hy = 7 } in
  let u = #{ ux = #{ proof = ghost_ "erased" }; uy = 8 } in
  Gc.full_major ();
  Printf.printf "%d %d\n" h.hy u.#uy
