open Oxsmt_core

exception Eval_error of string

let err fmt = Printf.ksprintf (fun s -> raise (Eval_error s)) fmt
let two = Bigint.of_int 2

(* [2^k], memoized in a monotonic cache filled incrementally (each power is one [mul] from
   its predecessor). Was an O(k) recompute per call; since [bit]/[to_bits] call [pow2 i]
   for every bit i in a per-bit loop, an And/Or/Xor/Ashr node was Sum_i O(i) = O(w^2)
   Bigint work. The cache makes each distinct power O(1) after first fill. BYTE-IDENTICAL:
   [pow2 k] returns the same value as before (a pure function of k); only its cost
   changes. *)
let pow2_tbl = Dynarray.create ()
let () = Dynarray.add_last pow2_tbl Bigint.one

let pow2 k =
  while Dynarray.length pow2_tbl <= k do
    Dynarray.add_last pow2_tbl (Bigint.mul (Dynarray.get_last pow2_tbl) two)
  done;
  Dynarray.get pow2_tbl k
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

(* OPEN-RECURSIVE bodies: sub-evaluation goes through [ev_bv]/[ev_bool] rather than direct
   recursion, so the SAME code is instantiated both memoized ({!eval_bv}) and unmemoized
   ({!eval_bv_unmemoized}, kept as the reference for the memoization-transparency test).
   Not exported. *)
let eval_bv_body ~ev_bv ~ev_bool defs ~lookup (term : Term.t) : Bigint.t * int =
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
        let v1 () = fst (ev_bv (List.nth args 0)) in
        let v2 () = fst (ev_bv (List.nth args 1)) in
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
           let hv, _ = ev_bv hi in
           let lv, lw = ev_bv lo in
           Bigint.add lv (Bigint.mul hv (pow2 lw))
         | Bv_op.Extract { hi; lo }, [ _ ] ->
           mask (fst (Bigint.divmod (v1 ()) (pow2 lo))) (hi - lo + 1)
         | Bv_op.Zero_extend _, [ _ ] -> v1 ()
         | Bv_op.Sign_extend k, [ a ] ->
           let av, aw = ev_bv a in
           if bit av (aw - 1)
           then Bigint.add av (Bigint.sub (pow2 (aw + k)) (pow2 aw))
           else av
         | op, _ -> err "eval: bad arity for %s in value position" (Bv_op.to_string op))
    in
    value, w
  | Ite (c, a, b) -> if ev_bool c then ev_bv a else ev_bv b
  | _ -> err "eval: non-application in bit-vector position"
;;

let eval_bool_body ~ev_bv ~ev_bool defs ~lookup (term : Term.t) : bool =
  match term.node with
  | Bool_const b -> b
  | Not a -> not (ev_bool a)
  | And args -> List.for_all ev_bool (Iarr.to_list args)
  | Or args -> List.exists ev_bool (Iarr.to_list args)
  | Ite (c, a, b) -> if ev_bool c then ev_bool a else ev_bool b
  | Eq (a, b) ->
    (match a.sort with
     | Sort.Bool -> Bool.equal (ev_bool a) (ev_bool b)
     | _ -> Bigint.equal (fst (ev_bv a)) (fst (ev_bv b)))
  | App (_sym, _) ->
    (match defs.Blast.classify term with
     | Some (Blast.Op (op, args, _)) ->
       let ub i = fst (ev_bv (List.nth args i)) in
       let sb i =
         let v, w = ev_bv (List.nth args i) in
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
  | Le _ | Arith _ | Real_arith _ | Int_const _ | Real_const _ ->
    err "eval: arithmetic atom (not QF_BV)"
;;

(* MEMOIZED entry points. The cache is a per-call FRESH [Term.Table] created here and
   never shared across calls: within one [eval_bv]/[eval_bool] call the model [lookup] is
   fixed, so a term's value is a pure function of the (hash-consed) term, and caching by
   term identity is fully transparent (identical results to {!eval_bv_unmemoized}, proven
   by the memoization-equivalence test). Memoization also collapses the [v1 ()]/[v2 ()]
   thunks, which otherwise re-evaluate an operand once per result bit — turning per-node
   O(w) and a shared DAG's exponential blow-up into linear. *)
let make_memoized defs ~lookup =
  let bvm : (Bigint.t * int) Term.Table.t = Term.Table.create 256 in
  let boolm : bool Term.Table.t = Term.Table.create 256 in
  let rec ev_bv t =
    match Term.Table.find_opt bvm t with
    | Some r -> r
    | None ->
      let r = eval_bv_body ~ev_bv ~ev_bool defs ~lookup t in
      Term.Table.replace bvm t r;
      r
  and ev_bool t =
    match Term.Table.find_opt boolm t with
    | Some r -> r
    | None ->
      let r = eval_bool_body ~ev_bv ~ev_bool defs ~lookup t in
      Term.Table.replace boolm t r;
      r
  in
  ev_bv, ev_bool
;;

let eval_bv defs ~lookup term =
  let ev_bv, _ = make_memoized defs ~lookup in
  ev_bv term
;;

let eval_bool defs ~lookup term =
  let _, ev_bool = make_memoized defs ~lookup in
  ev_bool term
;;

(* Unmemoized reference (the pre-memoization semantics), kept ONLY as the oracle for the
   memoization-transparency test. Not for production use — exponential on shared DAGs. *)
let make_unmemoized defs ~lookup =
  let rec ev_bv t = eval_bv_body ~ev_bv ~ev_bool defs ~lookup t
  and ev_bool t = eval_bool_body ~ev_bv ~ev_bool defs ~lookup t in
  ev_bv, ev_bool
;;

let eval_bv_unmemoized defs ~lookup term =
  let ev_bv, _ = make_unmemoized defs ~lookup in
  ev_bv term
;;

let eval_bool_unmemoized defs ~lookup term =
  let _, ev_bool = make_unmemoized defs ~lookup in
  ev_bool term
;;
