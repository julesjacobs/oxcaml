(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 llvm-backend;
 flags += " -O3 -llvm-backend";
 native;
*)

type r =
  { mutable i : int;
    mutable s : string
  }

let opaque x = Sys.opaque_identity x

let[@inline never] choose b x y = if opaque b then x else y

let[@inline never] make_pair x y = opaque x, opaque y

let[@inline never] direct_call x = opaque x + 1

let[@inline never] indirect_call f x = f (opaque x)

let[@inline never] rec sum_down n acc =
  if n = 0 then acc else sum_down (n - 1) (acc + n)

let[@inline never] get_int (a : int Atomic.t) = Atomic.get a

let[@inline never] get_string (a : string Atomic.t) = Atomic.get a

let[@inline never] compare_exchange_int (a : int Atomic.t) oldv newv =
  Atomic.compare_exchange a (opaque oldv) (opaque newv)

let[@inline never] compare_exchange_ref (a : int ref Atomic.t) oldv newv =
  Atomic.compare_exchange a (opaque oldv) (opaque newv)

let[@inline never] set_i r v = r.i <- opaque v

let[@inline never] set_s r v = r.s <- opaque v

let[@inline never] allocate_many n =
  let rec loop i acc =
    if i = 0 then acc else loop (i - 1) (make_pair i (i + 1) :: acc)
  in
  loop n []

let check label ok = if not ok then failwith label

let () =
  check "choose true" (choose true "left" "right" = "left");
  check "choose false" (choose false "left" "right" = "right");
  check "make_pair" (make_pair 20 22 = (20, 22));
  check "direct_call" (direct_call 41 = 42);
  check "indirect_call" (indirect_call direct_call 41 = 42);
  check "sum_down" (sum_down 1000 0 = 500500);
  let ai = Atomic.make 41 in
  Atomic.set ai 42;
  check "atomic int" (get_int ai = 42);
  let astr = Atomic.make "old" in
  Atomic.set astr "new";
  check "atomic string" (get_string astr = "new");
  let acx = Atomic.make 3 in
  check "atomic compare_exchange int success"
    (compare_exchange_int acx 3 4 = 3 && Atomic.get acx = 4);
  check "atomic compare_exchange int failure"
    (compare_exchange_int acx 3 5 = 4 && Atomic.get acx = 4);
  let r0 = ref 0 and r1 = ref 1 and r2 = ref 2 in
  let ar = Atomic.make r0 in
  check "atomic compare_exchange ref success"
    (compare_exchange_ref ar r0 r1 == r0 && Atomic.get ar == r1);
  check "atomic compare_exchange ref failure"
    (compare_exchange_ref ar r0 r2 == r1 && Atomic.get ar == r1);
  let r = { i = 0; s = "old" } in
  set_i r 17;
  set_s r "updated";
  check "set_i" (r.i = 17);
  check "set_s" (r.s = "updated");
  check "string equal" (String.equal (opaque "abc\000def") "abc\000def");
  check "string compare" (String.compare (opaque "\255") "\254" > 0);
  let b = Bytes.of_string "abc\000def" in
  Bytes.set b 3 'x';
  check "bytes mutation" (Bytes.equal b (Bytes.of_string "abcxdef"));
  let allocated = allocate_many 50_000 in
  Gc.minor ();
  check "allocation head" (List.hd allocated = (1, 2));
  check "allocation length" (List.length allocated = 50_000)
