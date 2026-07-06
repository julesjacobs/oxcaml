(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* pcell soundness probes: the attacks that must NOT verify.  The API
   is ascribed inline (same signature as pcell_lib, which the positive
   test lean_pcell.ml exercises); the token discipline is enforced by
   two layers, and each probe pins down which one fires:
   Lean (no supporting fact) for forged and cross-cell tokens, the
   uniqueness mode checker for duplication and stale reuse.  The
   token axioms are an embedded prelude block (no -vox-prelude
   flag). *)

[%%vox.lean {lean|
opaque cid : VoxU -> Int
opaque tid : VoxU -> Int
opaque cts : VoxU -> Int
|lean}]
[%%expect{|
|}]

module P : sig
  type icell
  type itoken
  type cpair = { cell : icell; tok : itoken }

  val alloc :
    (v : int) -> cpair{ tid _.tok = cid _.cell && cts _.tok = v } @ unique

  val read :
    (c : icell) -> (k : int) ->
    itoken{ tid _ = cid c && cts _ = k } @ unique ->
    (int{ _ = k } * itoken{ tid _ = cid c && cts _ = k }) @ unique

  val write :
    (c : icell) -> (old : int) -> (v : int) ->
    itoken{ tid _ = cid c && cts _ = old } @ unique ->
    itoken{ tid _ = cid c && cts _ = v } @ unique
end = struct
  type icell = { mutable v : int; id_ : int }
  type itoken = Tok of { id : int }
  type cpair = { cell : icell; tok : itoken }

  let ctr = ref 0

  let alloc :
    (v : int) -> cpair{ tid _.tok = cid _.cell && cts _.tok = v } @ unique =
    fun v ->
      incr ctr;
      let id = !ctr in
      let c = { v; id_ = id } in
      assume_unchecked_ { cell = c; tok = Tok { id } }

  let read :
    (c : icell) -> (k : int) ->
    itoken{ tid _ = cid c && cts _ = k } @ unique ->
    (int{ _ = k } * itoken{ tid _ = cid c && cts _ = k }) @ unique =
    fun c k t ->
      ignore t; ignore k;
      ( (assume_unchecked_ c.v : int{ _ = k }),
        (assume_unchecked_ (Tok { id = c.id_ })
          : itoken{ tid _ = cid c && cts _ = k }) )

  let write :
    (c : icell) -> (old : int) -> (v : int) ->
    itoken{ tid _ = cid c && cts _ = old } @ unique ->
    itoken{ tid _ = cid c && cts _ = v } @ unique =
    fun c _old v t ->
      ignore t; c.v <- v; assume_unchecked_ (Tok { id = c.id_ })
end
[%%expect{|
module P :
  sig
    type icell
    type itoken
    type cpair = { cell : icell; tok : itoken; }
    val alloc :
      (v : int) -> cpair{ tid _.tok = cid _.cell && cts _.tok = v } @ unique
    val read :
      (c : icell) ->
      (k : int) ->
      itoken{ tid _ = cid c && cts _ = k } @ unique ->
      int{ _ = k } * itoken{ tid _ = cid c && cts _ = k } @ unique
    val write :
      (c : icell) ->
      (old : int) ->
      (v : int) ->
      itoken{ tid _ = cid c && cts _ = old } @ unique ->
      itoken{ tid _ = cid c && cts _ = v } @ unique
  end
|}]

open P
[%%expect{|
|}]

(* Forged pair: repackage cell 1 with cell 2's token and coerce to the
   refined pair type.  tid t2 = cid c1 has no supporting fact. *)
let forge : (n : int) -> int =
  fun n ->
  let refine_ p = alloc n in
  let refine_ q = alloc n in
  let { cell = c1; tok = _t1 } = p in
  let { cell = _c2; tok = t2 } = q in
  let refine_ m =
    (refine_ { cell = c1; tok = t2 }
      : cpair{ tid _.tok = cid _.cell && cts _.tok = n })
  in
  ignore m;
  0
[%%expect{|
Line 8, characters 13-36:
8 |     (refine_ { cell = c1; tok = t2 }
                 ^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: tid (mk (c1, t2)).tok = cid (mk (c1, t2)).cell && cts (mk (c1, t2)).tok = n
Hypotheses:
  _c2 = q.cell
  t2 = q.tok
  c1 = p.cell
  _t1 = p.tok
  tid *unknown8*.tok = cid *unknown8*.cell && cts *unknown8*.tok = n
  q = *unknown8*
  tid q.tok = cid q.cell && cts q.tok = n
  tid *unknown7*.tok = cid *unknown7*.cell && cts *unknown7*.tok = n
  p = *unknown7*
  tid p.tok = cid p.cell && cts p.tok = n
(lean: error: `grind` failed)
|}]

(* Cross-cell confusion: coerce cell 1's token to claim it owns
   cell 2.  Unprovable even though both cells hold the same value:
   the cell-binding (tid), not the contents, is what blocks it. *)
let cross : (n : int) -> int =
  fun n ->
  let refine_ p = alloc n in
  let refine_ q = alloc n in
  let { cell = _c1; tok = t1 } = p in
  let { cell = c2; tok = _t2 } = q in
  let bad = (refine_ t1 : itoken{ tid _ = cid c2 && cts _ = n }) in
  ignore bad;
  0
[%%expect{|
Line 7, characters 21-23:
7 |   let bad = (refine_ t1 : itoken{ tid _ = cid c2 && cts _ = n }) in
                         ^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: tid t1 = cid c2 && cts t1 = n
Hypotheses:
  c2 = q.cell
  _t2 = q.tok
  _c1 = p.cell
  t1 = p.tok
  tid *unknown11*.tok = cid *unknown11*.cell && cts *unknown11*.tok = n
  q = *unknown11*
  tid q.tok = cid q.cell && cts q.tok = n
  tid *unknown10*.tok = cid *unknown10*.cell && cts *unknown10*.tok = n
  p = *unknown10*
  tid p.tok = cid p.cell && cts p.tok = n
(lean: error: `grind` failed)
|}]

(* Token duplication: destructure the pair twice and present both
   copies.  The mode checker links them. *)
let dup : (n : int) -> int{ _ = n + 1 } =
  fun n ->
  let refine_ p = alloc n in
  let { cell = c; tok = t } = p in
  let { cell = _; tok = t2 } = p in
  let n1 = n + 1 in
  let ua = write c n n1 t in
  ignore ua;
  let (rp, ub) = read c n t2 in
  ignore ub;
  rp + 1
[%%expect{|
Line 9, characters 26-28:
9 |   let (rp, ub) = read c n t2 in
                              ^^
Error: This value is used here, but it has already been used as unique at:
Line 7, characters 24-25:
7 |   let ua = write c n n1 t in
                            ^

|}]

(* Stale-token reuse after a write: consumed tokens cannot be
   presented again. *)
let stale : (n : int) -> int{ _ = n } =
  fun n ->
  let refine_ p = alloc n in
  let { cell = c; tok = t } = p in
  let u = write c n 0 t in
  ignore u;
  let (rp, u2) = read c n t in
  ignore u2;
  rp
[%%expect{|
Line 7, characters 26-27:
7 |   let (rp, u2) = read c n t in
                              ^
Error: This value is used here, but it has already been used as unique at:
Line 5, characters 22-23:
5 |   let u = write c n 0 t in
                          ^

|}]
