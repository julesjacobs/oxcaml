(* Exact-integer Hermite Normal Form. See hnf.mli.

   Row-reduction to HNF by elementary UNIMODULAR row operations (swap, negate, add an
   integer multiple of one row to another), applied in lockstep to a working copy of [A]
   and to an identity matrix [U]; the result satisfies [U * A = H]. All arithmetic is
   {!Oxsmt_core.Bigint} (arbitrary precision), so no operation overflows or raises — HNF
   entries can grow large ("coefficient blow-up") but never wrap. The degrade-to-[None] on
   a too-large problem is the caller's boundary (Rational→Bigint ingestion, the row/column
   caps here); this kernel itself is total over Bigint. *)

module B = Oxsmt_core.Bigint

type matrix = B.t array array

type t =
  { h : matrix
  ; u : matrix
  ; det_sign : int
  }

(* z3-parity named defaults (util/lp/lp_settings.h). The integration layer enforces these
   on the assembled constraint matrix; the kernel itself imposes no size limit. *)
let max_rows = 75
let max_cols = 150
let cut_period = 4
let copy (a : matrix) : matrix = Array.map Array.copy a
let rows (a : matrix) = Array.length a
let cols (a : matrix) = if Array.length a = 0 then 0 else Array.length a.(0)

let identity n : matrix =
  Array.init n (fun i -> Array.init n (fun j -> if i = j then B.one else B.zero))
;;

(* floor(a / b) for b > 0 (Bigint [divmod] truncates toward zero; the remainder carries
   the sign of [a], so a negative non-exact dividend needs the quotient nudged down by
   one). *)
let floordiv a b =
  let q, r = B.divmod a b in
  if B.sign r < 0 then B.sub q B.one else q
;;

let compute (a0 : matrix) : t =
  let m = rows a0 in
  let n = cols a0 in
  let a = copy a0 in
  let u = identity m in
  let det_sign = ref 1 in
  let swap p q =
    if p <> q
    then (
      let ta = a.(p) in
      a.(p) <- a.(q);
      a.(q) <- ta;
      let tu = u.(p) in
      u.(p) <- u.(q);
      u.(q) <- tu;
      det_sign := - !det_sign)
  in
  let negate p =
    a.(p) <- Array.map B.neg a.(p);
    u.(p) <- Array.map B.neg u.(p);
    det_sign := - !det_sign
  in
  (* row[dst] += f * row[src], on both [a] (n cols) and [u] (m cols). det unchanged. *)
  let addmul dst src f =
    if not (B.is_zero f)
    then (
      for j = 0 to n - 1 do
        a.(dst).(j) <- B.add a.(dst).(j) (B.mul f a.(src).(j))
      done;
      for j = 0 to m - 1 do
        u.(dst).(j) <- B.add u.(dst).(j) (B.mul f u.(src).(j))
      done)
  in
  let pivot = ref 0 in
  for col = 0 to n - 1 do
    if !pivot < m
    then (
      (* Reduce every row >= pivot in column [col] to a SINGLE nonzero (their gcd) by
         repeated Euclidean subtraction: pick the smallest-magnitude nonzero as reducer,
         knock every other nonzero below it, repeat. Terminates — the magnitudes strictly
         shrink, as in the Euclidean algorithm. *)
      let continue = ref true in
      while !continue do
        (* smallest-magnitude nonzero row >= pivot in this column, and count of nonzeros *)
        let reducer = ref (-1) in
        let nonzeros = ref 0 in
        for r = !pivot to m - 1 do
          if not (B.is_zero a.(r).(col))
          then (
            incr nonzeros;
            if !reducer < 0
               || B.compare (B.abs a.(r).(col)) (B.abs a.(!reducer).(col)) < 0
            then reducer := r)
        done;
        if !nonzeros <= 1
        then continue := false
        else
          for r = !pivot to m - 1 do
            if r <> !reducer && not (B.is_zero a.(r).(col))
            then (
              let q, _ = B.divmod a.(r).(col) a.(!reducer).(col) in
              addmul r !reducer (B.neg q))
          done
      done;
      (* the unique nonzero row >= pivot (if any) becomes the pivot row *)
      let p = ref (-1) in
      for r = !pivot to m - 1 do
        if not (B.is_zero a.(r).(col)) then p := r
      done;
      if !p >= 0
      then (
        swap !p !pivot;
        if B.sign a.(!pivot).(col) < 0 then negate !pivot;
        let pv = a.(!pivot).(col) in
        (* reduce entries ABOVE the pivot into [0, pv) *)
        for r = 0 to !pivot - 1 do
          let q = floordiv a.(r).(col) pv in
          addmul r !pivot (B.neg q)
        done;
        incr pivot))
  done;
  { h = a; u; det_sign = !det_sign }
;;

let mul (x : matrix) (y : matrix) : matrix =
  let p = rows x in
  let q = cols x in
  let r = cols y in
  Array.init p (fun i ->
    Array.init r (fun k ->
      let acc = ref B.zero in
      for j = 0 to q - 1 do
        acc := B.add !acc (B.mul x.(i).(j) y.(j).(k))
      done;
      !acc))
;;

let matrix_equal (x : matrix) (y : matrix) : bool =
  rows x = rows y
  && cols x = cols y
  && Array.for_all2 (fun rx ry -> Array.for_all2 B.equal rx ry) x y
;;

(* Structural HNF check: rows in echelon order (each nonzero row's leading column strictly
   right of the previous, zero rows only at the bottom), pivots positive, and every entry
   ABOVE a pivot reduced into [0, pivot). *)
let is_hnf (h : matrix) : bool =
  let m = rows h in
  let n = cols h in
  let lead = Array.make (max m 1) n in
  for i = 0 to m - 1 do
    let j = ref 0 in
    while !j < n && B.is_zero h.(i).(!j) do
      incr j
    done;
    lead.(i) <- !j
  done;
  let ok = ref true in
  (* echelon: a nonzero row's lead is strictly greater than the previous row's lead (which
     forces the previous row to be nonzero too, so no nonzero row follows a zero row). *)
  for i = 1 to m - 1 do
    if lead.(i) < n && not (lead.(i - 1) < lead.(i)) then ok := false
  done;
  for i = 0 to m - 1 do
    if lead.(i) < n
    then (
      let pv = h.(i).(lead.(i)) in
      if B.sign pv <= 0 then ok := false;
      for r = 0 to i - 1 do
        let e = h.(r).(lead.(i)) in
        if B.sign e < 0 || B.compare e pv >= 0 then ok := false
      done)
  done;
  !ok
;;

let verify (t : t) (a0 : matrix) : bool =
  (t.det_sign = 1 || t.det_sign = -1)
  && rows t.u = rows a0
  && cols t.u = rows a0
  && matrix_equal (mul t.u a0) t.h
  && is_hnf t.h
;;
