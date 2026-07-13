open Oxsmt_core
module Sat = Oxsmt_solver.Sat

type view =
  | Const of Bigint.t * int
  | Op of Bv_op.t * Term.t list * int option

type defs =
  { classify : Term.t -> view option
  ; width_of_sort : Sort.t -> int option
  }

exception Unsupported_bv of string

type t =
  { sat : Sat.t
  ; defs : defs
  ; tru : Sat.lit (* a literal forced true; [neg_lit tru] is forced false *)
  ; bv_cache : Sat.lit array Term.Table.t
  ; bool_cache : Sat.lit Term.Table.t
  ; mutable vars : (Term.t * Sat.lit array) list (* first-encounter order, reversed *)
  }

let create defs =
  let sat = Sat.create () in
  let v = Sat.new_var sat in
  let tru = Sat.pos v in
  Sat.add_clause sat [ tru ];
  { sat
  ; defs
  ; tru
  ; bv_cache = Term.Table.create 256
  ; bool_cache = Term.Table.create 256
  ; vars = []
  }
;;

let sat t = t.sat
let bv_vars t = List.rev t.vars
let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported_bv s)) fmt

(* {2 Literal-level gates with constant folding}

   Folding against the forced true/false literals is not an optimization frill: bit-
   vector constants blast to those literals, so folding is what keeps a circuit over a
   constant operand (add-with-constant, shift-by-constant, extends) from emitting a fresh
   variable and clauses for every trivially-determined bit. *)

let lit_eq a b =
  Sat.var_of_lit a = Sat.var_of_lit b
  && Bool.equal (Sat.sign_of_lit a) (Sat.sign_of_lit b)
;;

let is_true t l = lit_eq l t.tru
let is_false t l = lit_eq l (Sat.neg_lit t.tru)
let fresh t = Sat.pos (Sat.new_var t.sat)
let add t lits = Sat.add_clause t.sat lits
let mk_not l = Sat.neg_lit l

(* r <-> (a /\ b) *)
let mk_and2 t a b =
  if is_false t a || is_false t b
  then Sat.neg_lit t.tru
  else if is_true t a
  then b
  else if is_true t b
  then a
  else if lit_eq a b
  then a
  else if lit_eq a (mk_not b)
  then Sat.neg_lit t.tru
  else (
    let r = fresh t in
    add t [ mk_not r; a ];
    add t [ mk_not r; b ];
    add t [ r; mk_not a; mk_not b ];
    r)
;;

let mk_or2 t a b = mk_not (mk_and2 t (mk_not a) (mk_not b))

(* r <-> (a XOR b) *)
let mk_xor t a b =
  if is_false t a
  then b
  else if is_true t a
  then mk_not b
  else if is_false t b
  then a
  else if is_true t b
  then mk_not a
  else if lit_eq a b
  then Sat.neg_lit t.tru
  else if lit_eq a (mk_not b)
  then t.tru
  else (
    let r = fresh t in
    add t [ mk_not r; a; b ];
    add t [ mk_not r; mk_not a; mk_not b ];
    add t [ r; mk_not a; b ];
    add t [ r; a; mk_not b ];
    r)
;;

(* r <-> (a = b) for one bit *)
let mk_iff t a b = mk_not (mk_xor t a b)

(* r <-> (if c then a else b) *)
let mk_ite t c a b =
  if is_true t c
  then a
  else if is_false t c
  then b
  else if lit_eq a b
  then a
  else if is_true t a && is_false t b
  then c
  else if is_false t a && is_true t b
  then mk_not c
  else (
    let r = fresh t in
    add t [ mk_not c; mk_not a; r ];
    add t [ mk_not c; a; mk_not r ];
    add t [ c; mk_not b; r ];
    add t [ c; b; mk_not r ];
    r)
;;

let mk_and_list t = function
  | [] -> t.tru
  | [ l ] -> l
  | lits ->
    if List.exists (is_false t) lits
    then Sat.neg_lit t.tru
    else (
      let lits = List.filter (fun l -> not (is_true t l)) lits in
      match lits with
      | [] -> t.tru
      | [ l ] -> l
      | lits ->
        let r = fresh t in
        List.iter (fun l -> add t [ mk_not r; l ]) lits;
        add t (r :: List.map mk_not lits);
        r)
;;

let mk_or_list t lits = mk_not (mk_and_list t (List.map mk_not lits))

(* {2 Word-level circuits.  A word is a [Sat.lit array], index 0 = LSB. } *)

let width_of t (term : Term.t) =
  match t.defs.width_of_sort term.sort with
  | Some w when w > 0 -> w
  | _ -> unsupported "no bit-vector width for sort of term"
;;

(* Bits of a nonneg constant [v], low [w] bits (LSB first). *)
let const_bits t v w =
  let two = Bigint.of_int 2 in
  let bits = Array.make w (Sat.neg_lit t.tru) in
  let cur = ref (Bigint.abs v) in
  for i = 0 to w - 1 do
    let q, r = Bigint.divmod !cur two in
    if not (Bigint.is_zero r) then bits.(i) <- t.tru;
    cur := q
  done;
  bits
;;

let bitwise t f a b =
  let w = Array.length a in
  Array.init w (fun i -> f t a.(i) b.(i))
;;

(* full adder: returns (sum, carry_out) *)
let full_adder t a b cin =
  let axb = mk_xor t a b in
  let sum = mk_xor t axb cin in
  let cout = mk_or2 t (mk_and2 t a b) (mk_and2 t cin axb) in
  sum, cout
;;

(* ripple-carry a + b + cin_0, drop final carry (mod 2^w). *)
let ripple_add t a b cin0 =
  let w = Array.length a in
  let out = Array.make w (Sat.neg_lit t.tru) in
  let carry = ref cin0 in
  for i = 0 to w - 1 do
    let s, c = full_adder t a.(i) b.(i) !carry in
    out.(i) <- s;
    carry := c
  done;
  out
;;

let bv_add t a b = ripple_add t a b (Sat.neg_lit t.tru)

(* two's complement negate: ~x + 1 *)
let neg t a =
  let notx = Array.map mk_not a in
  ripple_add t notx (const_bits t Bigint.zero (Array.length a)) t.tru
;;

(* a - b = a + ~b + 1 *)
let sub t a b =
  let notb = Array.map mk_not b in
  ripple_add t a notb t.tru
;;

(* shift-add multiply, result truncated to width w (quadratic clauses; v1). *)
let mul t a b =
  let w = Array.length a in
  let acc = ref (const_bits t Bigint.zero w) in
  for i = 0 to w - 1 do
    (* partial_i = (b_i ? (a << i) : 0) *)
    let partial =
      Array.init w (fun j ->
        if j >= i then mk_and2 t b.(i) a.(j - i) else Sat.neg_lit t.tru)
    in
    acc := bv_add t !acc partial
  done;
  !acc
;;

(* {3 Shifts} — [fill] is the bit shifted in (false, or sign bit for ashr). *)

(* shift left by constant k *)
let shl_const t a k =
  let w = Array.length a in
  Array.init w (fun j -> if j >= k then a.(j - k) else Sat.neg_lit t.tru)
;;

(* logical/arith shift right by constant k; [fill] fills vacated high bits *)
let shr_const _t a k fill =
  let w = Array.length a in
  Array.init w (fun j -> if j + k < w then a.(j + k) else fill)
;;

(* Variable shift via a barrel/log-shifter over the shift-amount bits [b], total across
   the full width so amounts >= w collapse to the fill value (SMT-LIB total semantics).
   [dir_left]: shift direction. [fill]: the vacated-bit value. *)
let var_shift t a b ~dir_left ~fill =
  let w = Array.length a in
  let cur = ref a in
  (* stages that move bits within the word (2^i < w) *)
  let i = ref 0 in
  while 1 lsl !i < w do
    let amt = 1 lsl !i in
    let bi = if !i < Array.length b then b.(!i) else Sat.neg_lit t.tru in
    let shifted = if dir_left then shl_const t !cur amt else shr_const t !cur amt fill in
    cur := Array.init w (fun j -> mk_ite t bi shifted.(j) !cur.(j));
    incr i
  done;
  (* any set shift-amount bit at position i with 2^i >= w means amount >= w -> fill *)
  let overflow = ref (Sat.neg_lit t.tru) in
  for k = 0 to Array.length b - 1 do
    if 1 lsl k >= w then overflow := mk_or2 t !overflow b.(k)
  done;
  let filled = Array.make w fill in
  Array.init w (fun j -> mk_ite t !overflow filled.(j) !cur.(j))
;;

(* {3 Comparisons} — return a single literal. *)

(* unsigned a < b, folding LSB->MSB so the highest differing bit decides. *)
let ult t a b =
  let w = Array.length a in
  let acc = ref (Sat.neg_lit t.tru) in
  for j = 0 to w - 1 do
    (* a_j < b_j OR (a_j = b_j AND acc) *)
    let lt_here = mk_and2 t (mk_not a.(j)) b.(j) in
    let eq_here = mk_iff t a.(j) b.(j) in
    acc := mk_or2 t lt_here (mk_and2 t eq_here !acc)
  done;
  !acc
;;

let ule t a b =
  let w = Array.length a in
  let acc = ref t.tru in
  for j = 0 to w - 1 do
    let lt_here = mk_and2 t (mk_not a.(j)) b.(j) in
    let eq_here = mk_iff t a.(j) b.(j) in
    acc := mk_or2 t lt_here (mk_and2 t eq_here !acc)
  done;
  !acc
;;

(* signed compare = unsigned compare with the sign bits flipped. *)
let flip_sign _t a =
  let w = Array.length a in
  let a' = Array.copy a in
  a'.(w - 1) <- mk_not a.(w - 1);
  a'
;;

let slt t a b = ult t (flip_sign t a) (flip_sign t b)
let sle t a b = ule t (flip_sign t a) (flip_sign t b)

(* bit-vector equality -> single literal (all bits equal). *)
let bv_eq t a b =
  let w = Array.length a in
  mk_and_list t (List.init w (fun j -> mk_iff t a.(j) b.(j)))
;;

let zext t a k = Array.append a (Array.make k (Sat.neg_lit t.tru))

(* Euclidean division: introduce fresh quotient [q] and remainder [r] and constrain them
   so that, WHEN the divisor is nonzero, [a = b*q + r] exactly with [r <u b]. Two overflow
   guards make the w-bit encoding faithful (without them the wrap of b*q admits a spurious
   quotient, e.g. q=8 for a=0,b=2 at w=4):
   - b*q is formed at DOUBLE width and its high half is forced to 0 (b*q < 2^w);
   - the add b*q + r is checked at width w+1 against zero-extended a (carry-out 0). The
     constraint is a global definition (b=0 => vacuous), so asserting it unconditionally
     is sound wherever the div/rem term occurs. Returns [(q, r, is_zero_b)]. *)
let divmod_vars t a b =
  let w = Array.length a in
  let q = Array.init w (fun _ -> fresh t) in
  let r = Array.init w (fun _ -> fresh t) in
  let is_zero_b = mk_not (mk_or_list t (Array.to_list b)) in
  let prod2 = mul t (zext t b w) (zext t q w) in
  let no_ovf = mk_and_list t (List.init w (fun i -> mk_not prod2.(w + i))) in
  let prod_low = Array.sub prod2 0 w in
  let sum1 = ripple_add t (zext t prod_low 1) (zext t r 1) (Sat.neg_lit t.tru) in
  let sum_eq = bv_eq t sum1 (zext t a 1) in
  let r_lt_b = ult t r b in
  let ok = mk_and_list t [ no_ovf; sum_eq; r_lt_b ] in
  add t [ is_zero_b; ok ];
  q, r, is_zero_b
;;

(* bvudiv: b=0 -> all ones (SMT-LIB total semantics), else the quotient. *)
let udiv t a b =
  let q, _, zb = divmod_vars t a b in
  Array.init (Array.length a) (fun i -> mk_ite t zb t.tru q.(i))
;;

(* bvurem: b=0 -> a, else the remainder. *)
let urem t a b =
  let _, r, zb = divmod_vars t a b in
  Array.init (Array.length a) (fun i -> mk_ite t zb a.(i) r.(i))
;;

(* {2 Blasting the term DAG} *)

let rec bits t (term : Term.t) : Sat.lit array =
  match Term.Table.find_opt t.bv_cache term with
  | Some r -> r
  | None ->
    let r = bits_uncached t term in
    Term.Table.replace t.bv_cache term r;
    r

and bits_uncached t (term : Term.t) : Sat.lit array =
  let w = width_of t term in
  match term.node with
  | App (_sym, node_args) ->
    (match t.defs.classify term with
     | Some (Const (v, _)) -> const_bits t v w
     | Some (Op (op, args, _)) ->
       (match op, args with
        | Bv_op.Not, [ a ] -> Array.map mk_not (bits t a)
        | Bv_op.And, [ a; b ] -> bitwise t mk_and2 (bits t a) (bits t b)
        | Bv_op.Or, [ a; b ] -> bitwise t mk_or2 (bits t a) (bits t b)
        | Bv_op.Xor, [ a; b ] -> bitwise t mk_xor (bits t a) (bits t b)
        | Bv_op.Neg, [ a ] -> neg t (bits t a)
        | Bv_op.Add, [ a; b ] -> bv_add t (bits t a) (bits t b)
        | Bv_op.Sub, [ a; b ] -> sub t (bits t a) (bits t b)
        | Bv_op.Mul, [ a; b ] -> mul t (bits t a) (bits t b)
        | Bv_op.Shl, [ a; b ] ->
          var_shift t (bits t a) (bits t b) ~dir_left:true ~fill:(Sat.neg_lit t.tru)
        | Bv_op.Lshr, [ a; b ] ->
          var_shift t (bits t a) (bits t b) ~dir_left:false ~fill:(Sat.neg_lit t.tru)
        | Bv_op.Ashr, [ a; b ] ->
          let ab = bits t a in
          var_shift t ab (bits t b) ~dir_left:false ~fill:ab.(Array.length ab - 1)
        | Bv_op.Concat, [ hi; lo ] ->
          (* SMT-LIB: (concat hi lo), lo occupies the low bits *)
          Array.append (bits t lo) (bits t hi)
        | Bv_op.Extract { hi; lo }, [ a ] ->
          let ab = bits t a in
          Array.init (hi - lo + 1) (fun j -> ab.(lo + j))
        | Bv_op.Zero_extend k, [ a ] ->
          Array.append (bits t a) (Array.make k (Sat.neg_lit t.tru))
        | Bv_op.Sign_extend k, [ a ] ->
          let ab = bits t a in
          Array.append ab (Array.make k ab.(Array.length ab - 1))
        | Bv_op.Udiv, [ a; b ] -> udiv t (bits t a) (bits t b)
        | Bv_op.Urem, [ a; b ] -> urem t (bits t a) (bits t b)
        | op, _ ->
          unsupported
            "bit-vector op %s: unexpected arity in value position"
            (Bv_op.to_string op))
     | None ->
       if Iarr.length node_args = 0
       then (
         (* a free bit-vector variable: fresh bits, recorded for model read-back *)
         let vbits = Array.init w (fun _ -> fresh t) in
         t.vars <- (term, vbits) :: t.vars;
         vbits)
       else unsupported "uninterpreted function over bit-vectors (out of QF_BV)")
  | Ite (c, a, b) ->
    let cl = blast_bool t c in
    let ab = bits t a
    and bb = bits t b in
    Array.init w (fun j -> mk_ite t cl ab.(j) bb.(j))
  | _ -> unsupported "non-application in bit-vector position"

and blast_bool t (term : Term.t) : Sat.lit =
  match Term.Table.find_opt t.bool_cache term with
  | Some r -> r
  | None ->
    let r = blast_bool_uncached t term in
    Term.Table.replace t.bool_cache term r;
    r

and blast_bool_uncached t (term : Term.t) : Sat.lit =
  match term.node with
  | Bool_const true -> t.tru
  | Bool_const false -> Sat.neg_lit t.tru
  | Not a -> mk_not (blast_bool t a)
  | And args -> mk_and_list t (List.map (blast_bool t) (Iarr.to_list args))
  | Or args -> mk_or_list t (List.map (blast_bool t) (Iarr.to_list args))
  | Ite (c, a, b) -> mk_ite t (blast_bool t c) (blast_bool t a) (blast_bool t b)
  | Eq (a, b) ->
    (match a.sort with
     | Sort.Bool -> mk_iff t (blast_bool t a) (blast_bool t b)
     | _ when is_bv t a -> bv_eq t (bits t a) (bits t b)
     | _ -> unsupported "equality over a non-Bool non-bit-vector sort")
  | App (_sym, node_args) ->
    (match t.defs.classify term with
     | Some (Op (op, args, _)) ->
       (match op, args with
        | Bv_op.Ult, [ a; b ] -> ult t (bits t a) (bits t b)
        | Bv_op.Ule, [ a; b ] -> ule t (bits t a) (bits t b)
        | Bv_op.Ugt, [ a; b ] -> ult t (bits t b) (bits t a)
        | Bv_op.Uge, [ a; b ] -> ule t (bits t b) (bits t a)
        | Bv_op.Slt, [ a; b ] -> slt t (bits t a) (bits t b)
        | Bv_op.Sle, [ a; b ] -> sle t (bits t a) (bits t b)
        | Bv_op.Sgt, [ a; b ] -> slt t (bits t b) (bits t a)
        | Bv_op.Sge, [ a; b ] -> sle t (bits t b) (bits t a)
        | op, _ ->
          unsupported
            "bit-vector op %s in Bool position: not a predicate or bad arity"
            (Bv_op.to_string op))
     | Some (Const _) -> unsupported "bit-vector literal in Bool position"
     | None ->
       if Iarr.length node_args = 0
       then fresh t (* a free Boolean variable *)
       else unsupported "uninterpreted predicate (out of QF_BV)")
  | Le _ | Arith _ | Int_const _ -> unsupported "arithmetic atom (not QF_BV)"

and is_bv t (term : Term.t) =
  match t.defs.width_of_sort term.sort with
  | Some _ -> true
  | None -> false
;;

let assert_term t term = add t [ blast_bool t term ]
