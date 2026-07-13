open Oxsmt_core

exception Eval_error of string

let err fmt = Printf.ksprintf (fun s -> raise (Eval_error s)) fmt
let two = Bigint.of_int 2

let pow2 k =
  let rec go acc i = if i <= 0 then acc else go (Bigint.mul acc two) (i - 1) in
  go Bigint.one k
;;

(* [v] assumed nonneg; reduce to [0, 2^w). *)
let mask v w = snd (Bigint.divmod v (pow2 w))

let bit v i =
  let q, _ = Bigint.divmod v (pow2 i) in
  not (Bigint.is_zero (snd (Bigint.divmod q two)))
;;

let to_bits v w = Array.init w (fun i -> bit v i)

let of_bits bits =
  let acc = ref Bigint.zero in
  for i = Array.length bits - 1 downto 0 do
    acc := Bigint.add (Bigint.mul !acc two) (if bits.(i) then Bigint.one else Bigint.zero)
  done;
  !acc
;;

let signed v w = if bit v (w - 1) then Bigint.sub v (pow2 w) else v

(* shift amount as a saturating int: [>= w] if it does not fit or exceeds w. *)
let shift_amt s w =
  match Bigint.to_int_opt s with
  | Some k when k < w -> k
  | _ -> w
;;

let width_of defs (term : Term.t) =
  match defs.Blast.width_of_sort term.sort with
  | Some w -> w
  | None -> err "term is not bit-vector-sorted"
;;

let rec eval_bv defs ~lookup (term : Term.t) : Bigint.t * int =
  let w = width_of defs term in
  match term.node with
  | App (_sym, _) ->
    let value =
      match defs.Blast.classify term with
      | Some (Blast.Const (v, _)) -> mask v w
      | None ->
        (* a free bit-vector variable *)
        (match lookup term with
         | Some v -> mask v w
         | None -> err "unbound bit-vector variable")
      | Some (Blast.Op (op, args, _)) ->
        let v1 () = fst (eval_bv defs ~lookup (List.nth args 0)) in
        let v2 () = fst (eval_bv defs ~lookup (List.nth args 1)) in
        (match op, args with
         | Bv_op.Not, [ _ ] -> Bigint.sub (Bigint.sub (pow2 w) Bigint.one) (v1 ())
         | Bv_op.And, [ _; _ ] ->
           of_bits (Array.init w (fun i -> bit (v1 ()) i && bit (v2 ()) i))
         | Bv_op.Or, [ _; _ ] ->
           of_bits (Array.init w (fun i -> bit (v1 ()) i || bit (v2 ()) i))
         | Bv_op.Xor, [ _; _ ] ->
           of_bits (Array.init w (fun i -> bit (v1 ()) i <> bit (v2 ()) i))
         | Bv_op.Neg, [ _ ] -> mask (Bigint.sub (pow2 w) (v1 ())) w
         | Bv_op.Add, [ _; _ ] -> mask (Bigint.add (v1 ()) (v2 ())) w
         | Bv_op.Sub, [ _; _ ] ->
           mask (Bigint.add (v1 ()) (Bigint.sub (pow2 w) (v2 ()))) w
         | Bv_op.Mul, [ _; _ ] -> mask (Bigint.mul (v1 ()) (v2 ())) w
         | Bv_op.Udiv, [ _; _ ] ->
           let b = v2 () in
           if Bigint.is_zero b
           then Bigint.sub (pow2 w) Bigint.one
           else fst (Bigint.divmod (v1 ()) b)
         | Bv_op.Urem, [ _; _ ] ->
           let b = v2 () in
           if Bigint.is_zero b then v1 () else snd (Bigint.divmod (v1 ()) b)
         | Bv_op.Shl, [ _; _ ] ->
           let s = shift_amt (v2 ()) w in
           if s >= w then Bigint.zero else mask (Bigint.mul (v1 ()) (pow2 s)) w
         | Bv_op.Lshr, [ _; _ ] ->
           let s = shift_amt (v2 ()) w in
           if s >= w then Bigint.zero else fst (Bigint.divmod (v1 ()) (pow2 s))
         | Bv_op.Ashr, [ _; _ ] ->
           let a = to_bits (v1 ()) w in
           let s = shift_amt (v2 ()) w in
           let sign = a.(w - 1) in
           of_bits (Array.init w (fun j -> if j + s < w then a.(j + s) else sign))
         | Bv_op.Concat, [ hi; lo ] ->
           let hv, _ = eval_bv defs ~lookup hi in
           let lv, lw = eval_bv defs ~lookup lo in
           Bigint.add lv (Bigint.mul hv (pow2 lw))
         | Bv_op.Extract { hi; lo }, [ _ ] ->
           mask (fst (Bigint.divmod (v1 ()) (pow2 lo))) (hi - lo + 1)
         | Bv_op.Zero_extend _, [ _ ] -> v1 ()
         | Bv_op.Sign_extend k, [ a ] ->
           let av, aw = eval_bv defs ~lookup a in
           if bit av (aw - 1)
           then Bigint.add av (Bigint.sub (pow2 (aw + k)) (pow2 aw))
           else av
         | op, _ -> err "eval: bad arity for %s in value position" (Bv_op.to_string op))
    in
    value, w
  | Ite (c, a, b) ->
    if eval_bool defs ~lookup c then eval_bv defs ~lookup a else eval_bv defs ~lookup b
  | _ -> err "eval: non-application in bit-vector position"

and eval_bool defs ~lookup (term : Term.t) : bool =
  match term.node with
  | Bool_const b -> b
  | Not a -> not (eval_bool defs ~lookup a)
  | And args -> List.for_all (eval_bool defs ~lookup) (Iarr.to_list args)
  | Or args -> List.exists (eval_bool defs ~lookup) (Iarr.to_list args)
  | Ite (c, a, b) ->
    if eval_bool defs ~lookup c
    then eval_bool defs ~lookup a
    else eval_bool defs ~lookup b
  | Eq (a, b) ->
    (match a.sort with
     | Sort.Bool -> Bool.equal (eval_bool defs ~lookup a) (eval_bool defs ~lookup b)
     | _ -> Bigint.equal (fst (eval_bv defs ~lookup a)) (fst (eval_bv defs ~lookup b)))
  | App (_sym, _) ->
    (match defs.Blast.classify term with
     | Some (Blast.Op (op, args, _)) ->
       let ub i = fst (eval_bv defs ~lookup (List.nth args i)) in
       let sb i =
         let v, w = eval_bv defs ~lookup (List.nth args i) in
         signed v w
       in
       (match op, args with
        | Bv_op.Ult, [ _; _ ] -> Bigint.compare (ub 0) (ub 1) < 0
        | Bv_op.Ule, [ _; _ ] -> Bigint.compare (ub 0) (ub 1) <= 0
        | Bv_op.Ugt, [ _; _ ] -> Bigint.compare (ub 0) (ub 1) > 0
        | Bv_op.Uge, [ _; _ ] -> Bigint.compare (ub 0) (ub 1) >= 0
        | Bv_op.Slt, [ _; _ ] -> Bigint.compare (sb 0) (sb 1) < 0
        | Bv_op.Sle, [ _; _ ] -> Bigint.compare (sb 0) (sb 1) <= 0
        | Bv_op.Sgt, [ _; _ ] -> Bigint.compare (sb 0) (sb 1) > 0
        | Bv_op.Sge, [ _; _ ] -> Bigint.compare (sb 0) (sb 1) >= 0
        | _ -> err "eval: non-predicate op in Bool position")
     | Some (Blast.Const _) -> err "eval: bit-vector literal in Bool position"
     | None ->
       (match lookup term with
        | Some v -> not (Bigint.is_zero v)
        | None -> err "unbound Boolean variable"))
  | Le _ | Arith _ | Int_const _ -> err "eval: arithmetic atom (not QF_BV)"
;;
