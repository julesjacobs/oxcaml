(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 flags += " -O3 -llvm-backend";
 native;
*)

type r = { mutable i : int; mutable s : string }

let[@inline never] choose b x y = if b then x else y

let[@inline never] make_pair x y = x, y

let[@inline never] call f x = f x

let[@inline never] get_int (a : int Atomic.t) = Atomic.get a

let[@inline never] get_string (a : string Atomic.t) = Atomic.get a

let[@inline never] set_i r v = r.i <- v

let[@inline never] set_s r v = r.s <- v

let[@inline never] allocate_many n =
  let rec loop i acc =
    if i = 0 then acc else loop (i - 1) (make_pair i i :: acc)
  in
  loop n []

let () =
  assert (choose true "left" "right" = "left");
  assert (choose false "left" "right" = "right");
  assert (make_pair 20 22 = (20, 22));
  assert (call (fun x -> x + 1) 41 = 42);
  let ai = Atomic.make 41 in
  Atomic.set ai 42;
  assert (get_int ai = 42);
  let astr = Atomic.make "old" in
  Atomic.set astr "new";
  assert (get_string astr = "new");
  let r = { i = 0; s = "old" } in
  set_i r 17;
  set_s r "updated";
  assert (r.i = 17);
  assert (r.s = "updated");
  ignore (Sys.opaque_identity (allocate_many 50_000) : (int * int) list)
