(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "borrow_lib.mli borrow_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* RustHorn-style borrows (borrow_lib): every obligation below is
   really proved through Lean's grind.  The negative probes (double
   use, forged exports, frozen-loan writes, prophecy reuse, loan
   escape) live in mechanics/lean_borrow_fail.ml. *)

open Borrow_lib

(* Read-modify-write through a borrow; the residual's contents is
   revealed by mdrop's resolution and read back with proof. *)
let bump : (a : int) -> int{ _ = a + 1 } =
  fun a ->
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    let (r, m1) = mget m in              (* r = now m = x = a *)
    let m2 = mset m1 (r + 1) in
    let _u0 = mdrop m2 in         (* fin m2 = now m2 *)
    (() : unit{ p = a + 1 }))    (* export in lender terms *)
  in
  ignore u;                              (* u carried p = a + 1 *)
  let s = vpeek x' in                    (* s = x' = p *)
  s

(* Set in one branch to 2, in the other to 4, drop in both: the
   residual is provably even. *)
let branch_even : (a : int) -> (b : bool) -> int{ _ mod 2 = 0 } =
  fun a b ->
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    if b
    then (
      let m1 = mset m 2 in
      let _u0 = mdrop m1 in
      (() : unit{ p = 2 || p = 4 }))
    else (
      let m1 = mset m 4 in
      let _u0 = mdrop m1 in
      (() : unit{ p = 2 || p = 4 })))
  in
  ignore u;
  let s = vpeek x' in
  s

(* A helper the loan is passed through: adds k, preserves fin. *)
let add_through : (k : int) -> (m : mut) @ local unique ->
  mut{ now _ = now m + k && fin _ = fin m } @ local unique =
  fun k m -> exclave_ (
    let (r, m1) = mget m in
    let m2 = mset m1 (r + k) in
    (m2 : mut{ now _ = now m + k && fin _ = fin m }))

(* ...applied twice with dynamic increments. *)
let chain : (a : int) -> (k1 : int) -> (k2 : int) ->
  int{ _ = a + k1 + k2 } =
  fun a k1 k2 ->
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    let m1 = add_through k1 m in
    let m2 = add_through k2 m1 in
    let _u0 = mdrop m2 in
    (() : unit{ p = a + k1 + k2 }))
  in
  ignore u;
  let s = vpeek x' in
  s

(* Terminal ALIASED phase: peek then aliased drop on the same loan
   (aliased uses coexist; both freeze the loan against writes). *)
let bump_peeked : (a : int) -> int{ _ = a + 1 } =
  fun a ->
  let x = vnew a in
  let p = new_proph () in
  let (x', u) = borrow_mut p x (fun m ->
    let m1 = mset m (a + 1) in
    let _r = mpeek m1 in         (* aliased: _r = now m1 *)
    let _u0 = mdropa m1 in        (* aliased resolution *)
    (() : unit{ p = a + 1 }))
  in
  ignore u;
  let s = vpeek x' in
  s

(* Two refs, values crossing via vread + sequential borrows: a
   verified swap, summing the residuals. *)
let swap_refs : (a : int) -> (c : int) -> int{ _ = a + c } =
  fun a c ->
  let x = vnew a in
  let y = vnew c in
  let (xv, x1) = vread x in              (* xv = x = a *)
  let (yv, y1) = vread y in              (* yv = y = c *)
  let px = new_proph () in
  let (x', ux) = borrow_mut px x1 (fun m ->
    let m1 = mset m yv in        (* x := old y *)
    let _u0 = mdrop m1 in
    (() : unit{ px = c }))
  in
  let py = new_proph () in
  let (y', uy) = borrow_mut py y1 (fun m ->
    let m1 = mset m xv in        (* y := old x *)
    let _u0 = mdrop m1 in
    (() : unit{ py = a }))
  in
  ignore ux; ignore uy;
  let sx = vpeek x' in
  let sy = vpeek y' in
  sx + sy

(* TWO LIVE LOANS: nested borrows, the inner continuation captures
   the outer loan and bumps both; the inner residual flows to the top
   through 'b.  (A nested borrow_mut must not sit in tail position:
   a tail call's local closure argument would need the parent
   region.) *)
let two_live : (a : int) -> (c : int) -> int{ _ = a + c + 2 } =
  fun a c ->
  let x = vnew a in
  let y = vnew c in
  let px = new_proph () in
  let py = new_proph () in
  let (x', b) = borrow_mut px x (fun mx ->
    let res = borrow_mut py y (fun my ->
      let mx1 = add_through 1 mx in
      let my1 = add_through 1 my in
      let _u0 = mdrop mx1 in
      let _u1 = mdrop my1 in
      (() : unit{ px = a + 1 && py = c + 1 }))
    in
    res)
  in
  let (y', u) = b in
  ignore u;                              (* px = a+1 && py = c+1 *)
  let sx = vpeek x' in
  let sy = vpeek y' in
  sx + sy
