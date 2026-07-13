(* Fixed-width bitvector vocabulary (GOALS: bitvectors). See bv.mli for the representation
   and the collision-proof symbol-name namespace. *)

type op =
  | Bvnot
  | Bvand
  | Bvor
  | Bvxor
  | Bvneg
  | Bvadd
  | Bvsub
  | Bvmul
  | Bvudiv
  | Bvurem
  | Bvshl
  | Bvlshr
  | Bvashr
  | Bvult
  | Bvule
  | Bvslt
  | Bvsle
  | Concat
  | Extract of int * int
  | Zero_extend of int
  | Sign_extend of int

type view =
  | Const of
      { value : Bigint.t
      ; width : int
      }
  | Op of
      { op : op
      ; args : Term.t list
      ; result_width : int option
      }

(* The name namespace. Both bytes ['\\'] and ['|'] are illegal in every SMT-LIB symbol
   (simple or [|...|]-quoted), so no user-parsed symbol can ever intern to one of these,
   and the field separator ['|'] never occurs inside an encoded field (widths/indices are
   decimal ints; a literal value is a nonnegative {!Bigint.to_string}, digits only). *)
let marker = "\\bv|"
let is_bv_name name = String.starts_with ~prefix:marker name
let is_bv_sym sym = is_bv_name (Symbol.name sym)

let width_of_sort (s : Sort.t) =
  match s with
  | Sort.BitVec w -> Some w
  | Sort.Bool | Sort.Int _ | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.Array _ -> None
;;

let pow2 w =
  let two = Bigint.of_int 2 in
  let rec go acc k = if k = 0 then acc else go (Bigint.mul acc two) (k - 1) in
  go Bigint.one w
;;

(* Reduce [value] into [0, 2^width). [Bigint.divmod] truncates toward zero, so a negative
   remainder is lifted by one modulus. *)
let reduce value ~width =
  let m = pow2 width in
  let r = snd (Bigint.divmod value m) in
  if Bigint.sign r < 0 then Bigint.add r m else r
;;

let bits_lsb value ~width =
  let two = Bigint.of_int 2 in
  let v = ref (reduce value ~width) in
  let arr = Array.make (max 0 width) false in
  for i = 0 to width - 1 do
    let q, r = Bigint.divmod !v two in
    arr.(i) <- not (Bigint.is_zero r);
    v := q
  done;
  arr
;;

(* ---- decoding (pure): name -> view ---- *)

let op_view op args result_width = Some (Op { op; args; result_width })

let decode_op kw rest args =
  let i = int_of_string in
  let bv op w = op_view op args (Some w) in
  let pred op = op_view op args None in
  match kw, rest with
  | "bvnot", [ w ] -> bv Bvnot (i w)
  | "bvneg", [ w ] -> bv Bvneg (i w)
  | "bvand", [ w ] -> bv Bvand (i w)
  | "bvor", [ w ] -> bv Bvor (i w)
  | "bvxor", [ w ] -> bv Bvxor (i w)
  | "bvadd", [ w ] -> bv Bvadd (i w)
  | "bvsub", [ w ] -> bv Bvsub (i w)
  | "bvmul", [ w ] -> bv Bvmul (i w)
  | "bvudiv", [ w ] -> bv Bvudiv (i w)
  | "bvurem", [ w ] -> bv Bvurem (i w)
  | "bvshl", [ w ] -> bv Bvshl (i w)
  | "bvlshr", [ w ] -> bv Bvlshr (i w)
  | "bvashr", [ w ] -> bv Bvashr (i w)
  | "bvult", [ _ ] -> pred Bvult
  | "bvule", [ _ ] -> pred Bvule
  | "bvslt", [ _ ] -> pred Bvslt
  | "bvsle", [ _ ] -> pred Bvsle
  | "concat", [ w1; w2 ] -> bv Concat (i w1 + i w2)
  | "extract", [ hi; lo; _w ] -> bv (Extract (i hi, i lo)) (i hi - i lo + 1)
  | "zero_extend", [ n; w ] -> bv (Zero_extend (i n)) (i w + i n)
  | "sign_extend", [ n; w ] -> bv (Sign_extend (i n)) (i w + i n)
  | _ -> None
;;

let view (t : Term.t) =
  match t.Term.node with
  | Term.App (sym, args) ->
    let name = Symbol.name sym in
    if not (is_bv_name name)
    then None
    else (
      match String.split_on_char '|' name with
      | _prefix :: "lit" :: value_s :: [ w_s ] ->
        Some (Const { value = Bigint.of_string value_s; width = int_of_string w_s })
      | _prefix :: kw :: rest -> decode_op kw rest (Iarr.to_list args)
      | _ -> None)
  | Term.Bool_const _
  | Term.Int_const _
  | Term.Arith _
  | Term.Le _
  | Term.Eq _
  | Term.Not _
  | Term.And _
  | Term.Or _
  | Term.Ite _ -> None
;;

(* ---- construction ---- *)

let mint env name domain codomain = Env.declare_fun env name (Rank.create domain codomain)

let build_app ctx env name domain codomain args =
  let sym = mint env name domain codomain in
  Context.app ctx sym args
;;

let width_exn (x : Term.t) ~what =
  match x.Term.sort with
  | Sort.BitVec w -> w
  | Sort.Bool | Sort.Int _ | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.Array _ ->
    raise (Term.Sort_error (Printf.sprintf "Bv.%s: operand is not a bitvector" what))
;;

let const ctx env ~value ~width =
  if width < 1 then invalid_arg "Bv.const: width must be >= 1";
  let v = reduce value ~width in
  let name = Printf.sprintf "%slit|%s|%d" marker (Bigint.to_string v) width in
  let sym = mint env name [] (Sort.bitvec width) in
  Context.const ctx sym
;;

let unop ctx env op x =
  let w = width_exn x ~what:"unop" in
  let kw =
    match op with
    | Bvnot -> "bvnot"
    | Bvneg -> "bvneg"
    | Bvand
    | Bvor
    | Bvxor
    | Bvadd
    | Bvsub
    | Bvmul
    | Bvudiv
    | Bvurem
    | Bvshl
    | Bvlshr
    | Bvashr
    | Bvult
    | Bvule
    | Bvslt
    | Bvsle
    | Concat
    | Extract _
    | Zero_extend _
    | Sign_extend _ -> raise (Term.Sort_error "Bv.unop: operator is not unary")
  in
  let name = Printf.sprintf "%s%s|%d" marker kw w in
  build_app ctx env name [ Sort.bitvec w ] (Sort.bitvec w) [ x ]
;;

let binop ctx env op x y =
  let wx = width_exn x ~what:"binop" in
  let wy = width_exn y ~what:"binop" in
  if wx <> wy
  then
    raise
      (Term.Sort_error (Printf.sprintf "Bv.binop: operand widths differ (%d vs %d)" wx wy));
  let kw, result =
    match op with
    | Bvand -> "bvand", Sort.bitvec wx
    | Bvor -> "bvor", Sort.bitvec wx
    | Bvxor -> "bvxor", Sort.bitvec wx
    | Bvadd -> "bvadd", Sort.bitvec wx
    | Bvsub -> "bvsub", Sort.bitvec wx
    | Bvmul -> "bvmul", Sort.bitvec wx
    | Bvudiv -> "bvudiv", Sort.bitvec wx
    | Bvurem -> "bvurem", Sort.bitvec wx
    | Bvshl -> "bvshl", Sort.bitvec wx
    | Bvlshr -> "bvlshr", Sort.bitvec wx
    | Bvashr -> "bvashr", Sort.bitvec wx
    | Bvult -> "bvult", Sort.bool
    | Bvule -> "bvule", Sort.bool
    | Bvslt -> "bvslt", Sort.bool
    | Bvsle -> "bvsle", Sort.bool
    | Bvnot | Bvneg | Concat | Extract _ | Zero_extend _ | Sign_extend _ ->
      raise (Term.Sort_error "Bv.binop: operator is not an equal-width binary operator")
  in
  let name = Printf.sprintf "%s%s|%d" marker kw wx in
  build_app ctx env name [ Sort.bitvec wx; Sort.bitvec wx ] result [ x; y ]
;;

let concat ctx env hi lo =
  let w1 = width_exn hi ~what:"concat" in
  let w2 = width_exn lo ~what:"concat" in
  let name = Printf.sprintf "%sconcat|%d|%d" marker w1 w2 in
  build_app
    ctx
    env
    name
    [ Sort.bitvec w1; Sort.bitvec w2 ]
    (Sort.bitvec (w1 + w2))
    [ hi; lo ]
;;

let extract ctx env ~i ~j x =
  let w = width_exn x ~what:"extract" in
  if not (i >= j && j >= 0 && i < w)
  then
    raise
      (Term.Sort_error
         (Printf.sprintf "Bv.extract: indices [%d:%d] out of range for width %d" i j w));
  let name = Printf.sprintf "%sextract|%d|%d|%d" marker i j w in
  build_app ctx env name [ Sort.bitvec w ] (Sort.bitvec (i - j + 1)) [ x ]
;;

let zero_extend ctx env ~n x =
  let w = width_exn x ~what:"zero_extend" in
  if n < 0 then raise (Term.Sort_error "Bv.zero_extend: negative extension");
  let name = Printf.sprintf "%szero_extend|%d|%d" marker n w in
  build_app ctx env name [ Sort.bitvec w ] (Sort.bitvec (w + n)) [ x ]
;;

let sign_extend ctx env ~n x =
  let w = width_exn x ~what:"sign_extend" in
  if n < 0 then raise (Term.Sort_error "Bv.sign_extend: negative extension");
  let name = Printf.sprintf "%ssign_extend|%d|%d" marker n w in
  build_app ctx env name [ Sort.bitvec w ] (Sort.bitvec (w + n)) [ x ]
;;
