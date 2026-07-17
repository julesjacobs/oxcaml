open Oxsmt_core

type op =
  | Not
  | And
  | Or
  | Xor
  | Neg
  | Add
  | Sub
  | Mul
  | Udiv
  | Urem
  | Shl
  | Lshr
  | Ashr
  | Ult
  | Ule
  | Ugt
  | Uge
  | Slt
  | Sle
  | Sgt
  | Sge
  | Sdiv
  | Srem
  | Smod
  | Comp
  | Nand
  | Nor
  | Xnor
  | Concat
  | Extract of int * int
  | Zero_extend of int
  | Sign_extend of int
  | Rotate_left of int
  | Rotate_right of int
  | Repeat of int

type view =
  | Const of
      { bits : Bigint.t
      ; width : int
      }
  | Op of
      { op : op
      ; args : Term.t list
      }

type minter = string -> Rank.t -> Symbol.t

let marker = ".oxsmt.eval.bv|"
let is_name name = String.starts_with ~prefix:marker name

let op_name = function
  | Not -> "bvnot"
  | And -> "bvand"
  | Or -> "bvor"
  | Xor -> "bvxor"
  | Neg -> "bvneg"
  | Add -> "bvadd"
  | Sub -> "bvsub"
  | Mul -> "bvmul"
  | Udiv -> "bvudiv"
  | Urem -> "bvurem"
  | Shl -> "bvshl"
  | Lshr -> "bvlshr"
  | Ashr -> "bvashr"
  | Ult -> "bvult"
  | Ule -> "bvule"
  | Ugt -> "bvugt"
  | Uge -> "bvuge"
  | Slt -> "bvslt"
  | Sle -> "bvsle"
  | Sgt -> "bvsgt"
  | Sge -> "bvsge"
  | Sdiv -> "bvsdiv"
  | Srem -> "bvsrem"
  | Smod -> "bvsmod"
  | Comp -> "bvcomp"
  | Nand -> "bvnand"
  | Nor -> "bvnor"
  | Xnor -> "bvxnor"
  | Concat -> "concat"
  | Extract _ -> "extract"
  | Zero_extend _ -> "zero_extend"
  | Sign_extend _ -> "sign_extend"
  | Rotate_left _ -> "rotate_left"
  | Rotate_right _ -> "rotate_right"
  | Repeat _ -> "repeat"
;;

let pow2 width =
  let rec loop acc base n =
    if n = 0
    then acc
    else
      loop
        (if n land 1 = 1 then Bigint.mul acc base else acc)
        (if n = 1 then base else Bigint.mul base base)
        (n lsr 1)
  in
  loop Bigint.one (Bigint.of_int 2) width
;;

let reduce bits ~width =
  let modulus = pow2 width in
  let remainder = snd (Bigint.divmod bits modulus) in
  if Bigint.sign remainder < 0 then Bigint.add remainder modulus else remainder
;;

let checked_add ~what a b =
  if a < 0 || b < 0 || a > max_int - b
  then raise (Term.Sort_error (what ^ ": bit-vector width overflow"));
  a + b
;;

let checked_mul ~what a b =
  if a < 1 || b < 1 || a > max_int / b
  then raise (Term.Sort_error (what ^ ": bit-vector width overflow"));
  a * b
;;

let add_opt a b = if a < 0 || b < 0 || a > max_int - b then None else Some (a + b)
let mul_opt a b = if a < 1 || b < 1 || a > max_int / b then None else Some (a * b)

let width_exn ~what (term : Term.t) =
  match term.sort with
  | Sort.BitVec width -> width
  | Bool | Int _ | Uninterpreted _ | Datatype _ | Array _ ->
    raise (Term.Sort_error (what ^ ": operand is not a bit-vector"))
;;

let build_app ctx mint name domain codomain args =
  let symbol = mint name (Rank.create domain codomain) in
  Context.app ctx symbol args
;;

let const ctx mint ~bits ~width =
  if width < 1 then raise (Term.Sort_error "Bv_term.const: width must be positive");
  let bits = reduce bits ~width in
  let name = Printf.sprintf "%slit|%s|%d" marker (Bigint.to_string bits) width in
  let symbol = mint name (Rank.create [] (Sort.bitvec width)) in
  Context.const ctx symbol
;;

let unop ctx mint op arg =
  let width = width_exn ~what:"Bv_term.unop" arg in
  (match op with
   | Not | Neg -> ()
   | _ -> raise (Term.Sort_error "Bv_term.unop: operator is not unary"));
  let name = Printf.sprintf "%s%s|%d" marker (op_name op) width in
  build_app ctx mint name [ Sort.bitvec width ] (Sort.bitvec width) [ arg ]
;;

let is_predicate = function
  | Ult | Ule | Ugt | Uge | Slt | Sle | Sgt | Sge -> true
  | Not
  | And
  | Or
  | Xor
  | Neg
  | Add
  | Sub
  | Mul
  | Udiv
  | Urem
  | Shl
  | Lshr
  | Ashr
  | Sdiv
  | Srem
  | Smod
  | Comp
  | Nand
  | Nor
  | Xnor
  | Concat
  | Extract _
  | Zero_extend _
  | Sign_extend _
  | Rotate_left _
  | Rotate_right _
  | Repeat _ -> false
;;

let is_equal_width_binary = function
  | And
  | Or
  | Xor
  | Add
  | Sub
  | Mul
  | Udiv
  | Urem
  | Shl
  | Lshr
  | Ashr
  | Ult
  | Ule
  | Ugt
  | Uge
  | Slt
  | Sle
  | Sgt
  | Sge
  | Sdiv
  | Srem
  | Smod
  | Comp
  | Nand
  | Nor
  | Xnor -> true
  | Not
  | Neg
  | Concat
  | Extract _
  | Zero_extend _
  | Sign_extend _
  | Rotate_left _
  | Rotate_right _
  | Repeat _ -> false
;;

let binop ctx mint op left right =
  if not (is_equal_width_binary op)
  then raise (Term.Sort_error "Bv_term.binop: operator is not binary");
  let left_width = width_exn ~what:"Bv_term.binop" left in
  let right_width = width_exn ~what:"Bv_term.binop" right in
  if left_width <> right_width
  then raise (Term.Sort_error "Bv_term.binop: operand widths differ");
  let result_sort =
    if is_predicate op
    then Sort.bool
    else if op = Comp
    then Sort.bitvec 1
    else Sort.bitvec left_width
  in
  let name = Printf.sprintf "%s%s|%d" marker (op_name op) left_width in
  build_app
    ctx
    mint
    name
    [ Sort.bitvec left_width; Sort.bitvec left_width ]
    result_sort
    [ left; right ]
;;

let concat ctx mint high low =
  let high_width = width_exn ~what:"Bv_term.concat" high in
  let low_width = width_exn ~what:"Bv_term.concat" low in
  let result_width = checked_add ~what:"Bv_term.concat" high_width low_width in
  let name = Printf.sprintf "%sconcat|%d|%d" marker high_width low_width in
  build_app
    ctx
    mint
    name
    [ Sort.bitvec high_width; Sort.bitvec low_width ]
    (Sort.bitvec result_width)
    [ high; low ]
;;

let extract ctx mint ~i ~j arg =
  let width = width_exn ~what:"Bv_term.extract" arg in
  if i < j || j < 0 || i >= width
  then raise (Term.Sort_error "Bv_term.extract: indices are out of range");
  let result_width = i - j + 1 in
  let name = Printf.sprintf "%sextract|%d|%d|%d" marker i j width in
  build_app ctx mint name [ Sort.bitvec width ] (Sort.bitvec result_width) [ arg ]
;;

let extend ctx mint op n arg =
  if n < 0 then raise (Term.Sort_error "Bv_term.extend: negative extension");
  let width = width_exn ~what:"Bv_term.extend" arg in
  let result_width = checked_add ~what:"Bv_term.extend" width n in
  let name = Printf.sprintf "%s%s|%d|%d" marker (op_name op) n width in
  build_app ctx mint name [ Sort.bitvec width ] (Sort.bitvec result_width) [ arg ]
;;

let zero_extend ctx mint ~n arg = extend ctx mint (Zero_extend n) n arg
let sign_extend ctx mint ~n arg = extend ctx mint (Sign_extend n) n arg

let rotate ctx mint op n arg =
  if n < 0 then raise (Term.Sort_error "Bv_term.rotate: negative rotation");
  let width = width_exn ~what:"Bv_term.rotate" arg in
  let name = Printf.sprintf "%s%s|%d|%d" marker (op_name op) n width in
  build_app ctx mint name [ Sort.bitvec width ] (Sort.bitvec width) [ arg ]
;;

let rotate_left ctx mint ~n arg = rotate ctx mint (Rotate_left n) n arg
let rotate_right ctx mint ~n arg = rotate ctx mint (Rotate_right n) n arg

let repeat ctx mint ~n arg =
  let width = width_exn ~what:"Bv_term.repeat" arg in
  let result_width = checked_mul ~what:"Bv_term.repeat" n width in
  let name = Printf.sprintf "%srepeat|%d|%d" marker n width in
  build_app ctx mint name [ Sort.bitvec width ] (Sort.bitvec result_width) [ arg ]
;;

(* The decoder is deliberately stricter than the namespace admission predicate.  A
   marker is meaningful only when every encoded field agrees with the actual term. *)

let int_fields fields = List.map int_of_string_opt fields

let one_bv_arg width result args =
  match args with
  | [ (arg : Term.t) ] ->
    width >= 1
    && Sort.equal arg.sort (Sort.bitvec width)
    && Sort.equal result (Sort.bitvec width)
  | _ -> false
;;

let two_bv_args width args =
  match args with
  | [ (left : Term.t); (right : Term.t) ] ->
    width >= 1
    && Sort.equal left.sort (Sort.bitvec width)
    && Sort.equal right.sort (Sort.bitvec width)
  | _ -> false
;;

let simple_op keyword =
  match keyword with
  | "bvnot" -> Some Not
  | "bvand" -> Some And
  | "bvor" -> Some Or
  | "bvxor" -> Some Xor
  | "bvneg" -> Some Neg
  | "bvadd" -> Some Add
  | "bvsub" -> Some Sub
  | "bvmul" -> Some Mul
  | "bvudiv" -> Some Udiv
  | "bvurem" -> Some Urem
  | "bvshl" -> Some Shl
  | "bvlshr" -> Some Lshr
  | "bvashr" -> Some Ashr
  | "bvult" -> Some Ult
  | "bvule" -> Some Ule
  | "bvugt" -> Some Ugt
  | "bvuge" -> Some Uge
  | "bvslt" -> Some Slt
  | "bvsle" -> Some Sle
  | "bvsgt" -> Some Sgt
  | "bvsge" -> Some Sge
  | "bvsdiv" -> Some Sdiv
  | "bvsrem" -> Some Srem
  | "bvsmod" -> Some Smod
  | "bvcomp" -> Some Comp
  | "bvnand" -> Some Nand
  | "bvnor" -> Some Nor
  | "bvxnor" -> Some Xnor
  | _ -> None
;;

let decode_simple keyword fields args result =
  match simple_op keyword, int_fields fields with
  | Some (Not as op), [ Some width ] | Some (Neg as op), [ Some width ] ->
    if one_bv_arg width result args then Some (Op { op; args }) else None
  | Some op, [ Some width ] when is_equal_width_binary op && two_bv_args width args ->
    let expected_result =
      if is_predicate op
      then Sort.bool
      else if op = Comp
      then Sort.bitvec 1
      else Sort.bitvec width
    in
    if Sort.equal result expected_result then Some (Op { op; args }) else None
  | _ -> None
;;

let decode_concat fields args result =
  match int_fields fields, args with
  | [ Some high_width; Some low_width ], [ (high : Term.t); (low : Term.t) ] ->
    (match add_opt high_width low_width with
     | Some result_width
       when high_width >= 1
            && low_width >= 1
            && Sort.equal high.sort (Sort.bitvec high_width)
            && Sort.equal low.sort (Sort.bitvec low_width)
            && Sort.equal result (Sort.bitvec result_width) ->
       Some (Op { op = Concat; args })
     | _ -> None)
  | _ -> None
;;

let decode_extract fields args result =
  match int_fields fields, args with
  | [ Some high; Some low; Some width ], [ (arg : Term.t) ]
    when width >= 1 && high >= low && low >= 0 && high < width ->
    let result_width = high - low + 1 in
    if
      Sort.equal arg.sort (Sort.bitvec width)
      && Sort.equal result (Sort.bitvec result_width)
    then Some (Op { op = Extract (high, low); args })
    else None
  | _ -> None
;;

let decode_extend op fields args result =
  match int_fields fields, args with
  | [ Some amount; Some width ], [ (arg : Term.t) ] when amount >= 0 && width >= 1 ->
    (match add_opt amount width with
     | Some result_width
       when Sort.equal arg.sort (Sort.bitvec width)
            && Sort.equal result (Sort.bitvec result_width) ->
       Some (Op { op = op amount; args })
     | _ -> None)
  | _ -> None
;;

let decode_rotate op fields args result =
  match int_fields fields, args with
  | [ Some amount; Some width ], [ (arg : Term.t) ]
    when amount >= 0
         && width >= 1
         && Sort.equal arg.sort (Sort.bitvec width)
         && Sort.equal result (Sort.bitvec width) -> Some (Op { op = op amount; args })
  | _ -> None
;;

let decode_repeat fields args result =
  match int_fields fields, args with
  | [ Some count; Some width ], [ (arg : Term.t) ] ->
    (match mul_opt count width with
     | Some result_width
       when Sort.equal arg.sort (Sort.bitvec width)
            && Sort.equal result (Sort.bitvec result_width) ->
       Some (Op { op = Repeat count; args })
     | _ -> None)
  | _ -> None
;;

let decode_const bits_s width_s result nargs =
  match int_of_string_opt width_s with
  | Some width when width >= 1 && nargs = 0 && Sort.equal result (Sort.bitvec width) ->
    (match Bigint.of_string bits_s with
     | bits when Bigint.sign bits >= 0 && Bigint.compare bits (pow2 width) < 0 ->
       Some (Const { bits; width })
     | _ -> None
     | exception Invalid_argument _ -> None)
  | _ -> None
;;

let view (term : Term.t) =
  match term.node with
  | Term.App (symbol, arg_array) ->
    let name = Symbol.name symbol in
    if not (is_name name)
    then None
    else (
      let args = Iarr.to_list arg_array in
      match String.split_on_char '|' name with
      | [ _prefix; "lit"; bits_s; width_s ] ->
        decode_const bits_s width_s term.sort (List.length args)
      | _prefix :: keyword :: fields ->
        (match keyword with
         | "concat" -> decode_concat fields args term.sort
         | "extract" -> decode_extract fields args term.sort
         | "zero_extend" -> decode_extend (fun n -> Zero_extend n) fields args term.sort
         | "sign_extend" -> decode_extend (fun n -> Sign_extend n) fields args term.sort
         | "rotate_left" -> decode_rotate (fun n -> Rotate_left n) fields args term.sort
         | "rotate_right" -> decode_rotate (fun n -> Rotate_right n) fields args term.sort
         | "repeat" -> decode_repeat fields args term.sort
         | _ -> decode_simple keyword fields args term.sort)
      | _ -> None)
  | Bool_const _ | Int_const _ | Arith _ | Le _ | Eq _ | Not _ | And _ | Or _ | Ite _ ->
    None
;;
