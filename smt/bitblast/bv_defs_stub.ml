open Oxsmt_core

type t =
  { env : Env.t
  ; widths : (Symbol.t, int) Hashtbl.t (* BV sort symbol -> width *)
  ; ops : (Symbol.t, Bv_op.t) Hashtbl.t (* App symbol -> operator *)
  ; sorts : (int, Sort.t) Hashtbl.t (* width -> its BV sort (interned once) *)
  }

let create env =
  { env; widths = Hashtbl.create 16; ops = Hashtbl.create 64; sorts = Hashtbl.create 16 }
;;

let width_of_sort t (s : Sort.t) =
  match s with
  | Sort.Uninterpreted sym -> Hashtbl.find_opt t.widths sym
  | _ -> None
;;

let defs t : Blast.defs =
  { op_of_sym = (fun sym -> Hashtbl.find_opt t.ops sym); width_of_sort = width_of_sort t }
;;

let sort t w =
  match Hashtbl.find_opt t.sorts w with
  | Some s -> s
  | None ->
    let sym = Env.declare_sort t.env (Printf.sprintf "BitVec.%d" w) in
    Hashtbl.replace t.widths sym w;
    let s = Sort.uninterpreted sym in
    Hashtbl.replace t.sorts w s;
    s
;;

let width_of_term t (term : Term.t) =
  match width_of_sort t term.sort with
  | Some w -> w
  | None -> invalid_arg "bv_defs_stub: term is not bit-vector-sorted"
;;

let declare_op t name rank bvop =
  let sym = Env.declare_fun t.env name rank in
  Hashtbl.replace t.ops sym bvop;
  sym
;;

let var t ctx name w =
  let s = sort t w in
  let sym = declare_op t name (Rank.create [] s) Bv_op.Var in
  Context.app ctx sym []
;;

let const t ctx v w =
  let s = sort t w in
  (* one symbol per (value, width); reduce v mod 2^w for a canonical name *)
  let two = Bigint.of_int 2 in
  let modulus =
    let rec pow acc i = if i = 0 then acc else pow (Bigint.mul acc two) (i - 1) in
    pow Bigint.one w
  in
  let _, vred = Bigint.divmod (Bigint.add (Bigint.abs v) modulus) modulus in
  let name = Printf.sprintf "bvc.%s.%d" (Bigint.to_string vred) w in
  let sym = declare_op t name (Rank.create [] s) (Bv_op.Const vred) in
  Context.app ctx sym []
;;

let result_sort t bvop arg_ws result_width =
  match bvop with
  | Bv_op.Ult
  | Bv_op.Ule
  | Bv_op.Ugt
  | Bv_op.Uge
  | Bv_op.Slt
  | Bv_op.Sle
  | Bv_op.Sgt
  | Bv_op.Sge -> Sort.bool
  | _ ->
    let w =
      match result_width with
      | Some w -> w
      | None ->
        (match bvop with
         | Bv_op.Concat -> List.fold_left ( + ) 0 arg_ws
         | Bv_op.Extract { hi; lo } -> hi - lo + 1
         | Bv_op.Zero_extend k | Bv_op.Sign_extend k -> List.hd arg_ws + k
         | _ -> List.hd arg_ws)
    in
    sort t w
;;

let op t ctx ?result_width bvop args =
  let arg_ws = List.map (width_of_term t) args in
  let res = result_sort t bvop arg_ws result_width in
  let dom = List.map (fun a -> a.Term.sort) args in
  let imm =
    match bvop with
    | Bv_op.Extract { hi; lo } -> Printf.sprintf "_%d_%d" hi lo
    | Bv_op.Zero_extend k | Bv_op.Sign_extend k -> Printf.sprintf "_%d" k
    | _ -> ""
  in
  let name =
    Printf.sprintf
      "bv.%s%s.%s"
      (Bv_op.to_string bvop)
      imm
      (String.concat "_" (List.map string_of_int arg_ws))
  in
  let sym = declare_op t name (Rank.create dom res) bvop in
  Context.app ctx sym args
;;
