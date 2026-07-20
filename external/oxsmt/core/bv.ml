(* Fixed-width bitvector vocabulary (GOALS: bitvectors). See bv.mli for the representation
   and the collision-proof reserved symbol-name namespace (board #58). *)

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

(* The name namespace (board #58). Bitvector symbols live in the reserved [.oxsmt.bv.*]
   sub-namespace: the public declaration doors ([Env.declare_fun]/[declare_sort],
   [Session.declare_fun]/[declare_sort]) REJECT any [.oxsmt.*] name and the SMT-LIB reader
   rejects a user declaration of one, so no user symbol can ever collide with a bitvector
   one even though interning is by name. Minting is cap-gated ([Env.declare_reserved]), so
   only a cap holder (the [Session], threaded to the builders as a minter closure) can
   create one. The ['|'] field separator never occurs inside an encoded field (widths and
   indices are decimal ints; a literal value is a nonnegative {!Bigint.to_string}, digits
   only), so splitting a name on ['|'] is unambiguous. (The prior scheme prefixed the name
   with the two bytes ['\\'] and ['|'] that no SMT-LIB symbol may contain; that byte class
   is retained only as defense-in-depth at the public doors — the [.oxsmt.] prefix plus
   the cap is the primary collision guard now.) *)
let marker = ".oxsmt.bv|"
let is_bv_name name = String.starts_with ~prefix:marker name
let is_bv_sym sym = is_bv_name (Symbol.name sym)

let width_of_sort (s : Sort.t) =
  match s with
  | Sort.BitVec w -> Some w
  | Sort.Bool | Sort.Int _ | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.Array _ | Sort.Real ->
    None
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

(* ---- decoding (pure): name -> view ----

   The decode is a pure function of the symbol NAME, but board #58 makes the name a
   reserved [.oxsmt.bv.*] one minted only through the cap-gated [Env.declare_reserved] /
   [Session.internal_minter]. A caller with that authority (the parser threads it; it is
   also public on {!Session}) could mint a marker name whose recorded RANK disagrees with
   the name's decoded operator/widths — e.g. [".oxsmt.bv|bvadd|1"] ranked over [BitVec 2]
   — and the bit-blaster would then impose width-1 addition on width-2 operands, a WRONG
   verdict. So every arm CROSS-CHECKS the decoded op's operand and result sorts against
   the term's ACTUAL argument sorts and result sort (arity included); any mismatch returns
   [None], so the symbol is treated as an ordinary uninterpreted function (the
   combinator's fail-closed path, at worst [unknown]) rather than reinterpreted. A
   legitimately-built bv term — the smart constructors below, or a mint whose rank agrees
   with the name — always matches, so this is inert on real bit-vectors. Width fields
   parse with [int_of_string_opt] so a malformed forged name is [None], never a raise. *)

let op_view op args result_width = Some (Op { op; args; result_width })
let bvw w = Sort.bitvec w
let all_bv w args = List.for_all (fun (a : Term.t) -> Sort.equal a.Term.sort (bvw w)) args

let decode_op kw rest args ~result =
  (* [bvres]/[pred] validate an equal-width n-ary op (result BitVec w / Bool); the
     caller's [when] guard fixes the arity via the [args] list shape. *)
  let bvres op w =
    if all_bv w args && Sort.equal result (bvw w) then op_view op args (Some w) else None
  in
  let pred op w =
    if all_bv w args && Sort.equal result Sort.bool then op_view op args None else None
  in
  let one =
    match args with
    | [ _ ] -> true
    | _ -> false
  in
  let two =
    match args with
    | [ _; _ ] -> true
    | _ -> false
  in
  match kw, List.map int_of_string_opt rest with
  | "bvnot", [ Some w ] when one -> bvres Bvnot w
  | "bvneg", [ Some w ] when one -> bvres Bvneg w
  | "bvand", [ Some w ] when two -> bvres Bvand w
  | "bvor", [ Some w ] when two -> bvres Bvor w
  | "bvxor", [ Some w ] when two -> bvres Bvxor w
  | "bvadd", [ Some w ] when two -> bvres Bvadd w
  | "bvsub", [ Some w ] when two -> bvres Bvsub w
  | "bvmul", [ Some w ] when two -> bvres Bvmul w
  | "bvudiv", [ Some w ] when two -> bvres Bvudiv w
  | "bvurem", [ Some w ] when two -> bvres Bvurem w
  | "bvshl", [ Some w ] when two -> bvres Bvshl w
  | "bvlshr", [ Some w ] when two -> bvres Bvlshr w
  | "bvashr", [ Some w ] when two -> bvres Bvashr w
  | "bvult", [ Some w ] when two -> pred Bvult w
  | "bvule", [ Some w ] when two -> pred Bvule w
  | "bvslt", [ Some w ] when two -> pred Bvslt w
  | "bvsle", [ Some w ] when two -> pred Bvsle w
  | "concat", [ Some w1; Some w2 ] ->
    (match args with
     | [ (a : Term.t); (b : Term.t) ]
       when Sort.equal a.Term.sort (bvw w1)
            && Sort.equal b.Term.sort (bvw w2)
            && Sort.equal result (bvw (w1 + w2)) -> op_view Concat args (Some (w1 + w2))
     | _ -> None)
  | "extract", [ Some hi; Some lo; Some w ] ->
    (match args with
     | [ (a : Term.t) ]
       when hi >= lo
            && lo >= 0
            && hi < w
            && Sort.equal a.Term.sort (bvw w)
            && Sort.equal result (bvw (hi - lo + 1)) ->
       op_view (Extract (hi, lo)) args (Some (hi - lo + 1))
     | _ -> None)
  | "zero_extend", [ Some n; Some w ] ->
    (match args with
     | [ (a : Term.t) ]
       when n >= 0 && Sort.equal a.Term.sort (bvw w) && Sort.equal result (bvw (w + n)) ->
       op_view (Zero_extend n) args (Some (w + n))
     | _ -> None)
  | "sign_extend", [ Some n; Some w ] ->
    (match args with
     | [ (a : Term.t) ]
       when n >= 0 && Sort.equal a.Term.sort (bvw w) && Sort.equal result (bvw (w + n)) ->
       op_view (Sign_extend n) args (Some (w + n))
     | _ -> None)
  | _ -> None
;;

let decode_lit ~value_s ~w_s ~result ~nargs =
  match int_of_string_opt w_s with
  | Some width when width >= 1 && nargs = 0 && Sort.equal result (bvw width) ->
    (match Bigint.of_string value_s with
     | value when Bigint.sign value >= 0 && Bigint.compare value (pow2 width) < 0 ->
       Some (Const { value; width })
     | _ -> None
     | exception Invalid_argument _ -> None)
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
      | [ _prefix; "lit"; value_s; w_s ] ->
        decode_lit ~value_s ~w_s ~result:t.Term.sort ~nargs:(Iarr.length args)
      | _prefix :: kw :: rest -> decode_op kw rest (Iarr.to_list args) ~result:t.Term.sort
      | _ -> None)
  | Term.Bool_const _
  | Term.Int_const _
  | Term.Real_const _
  | Term.Arith _
  | Term.Real_arith _
  | Term.Le _
  | Term.Eq _
  | Term.Not _
  | Term.And _
  | Term.Or _
  | Term.Ite _ -> None
;;

(* ---- construction ---- *)

type minter = string -> Rank.t -> Symbol.t

(* [mint] is the cap-backed reserved-symbol minter ([Env.declare_reserved cap env] closed
   over the session's private cap, board #58). The vocabulary lives under the reserved
   [.oxsmt.bv.*] prefix, which the public declaration doors reject, so the builders MUST
   mint through this capability rather than [Env.declare_fun] (which would reject the
   name). A [Session]-driven parse threads [Session.parse_minter] (an opaque
   [Internal_minter.t], applied via [Internal_minter.mint]); a standalone [Parser.parse]
   threads a minter over its own capped env. The closure is the least authority that does
   the job — the builder never holds the cap. *)
let build_app ctx mint name domain codomain args =
  let sym = mint name (Rank.create domain codomain) in
  Context.app ctx sym args
;;

let width_exn (x : Term.t) ~what =
  match x.Term.sort with
  | Sort.BitVec w -> w
  | Sort.Bool | Sort.Int _ | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.Array _ | Sort.Real ->
    raise (Term.Sort_error (Printf.sprintf "Bv.%s: operand is not a bitvector" what))
;;

let const ctx mint ~value ~width =
  if width < 1 then invalid_arg "Bv.const: width must be >= 1";
  let v = reduce value ~width in
  let name = Printf.sprintf "%slit|%s|%d" marker (Bigint.to_string v) width in
  let sym = mint name (Rank.create [] (Sort.bitvec width)) in
  Context.const ctx sym
;;

let unop ctx mint op x =
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
  build_app ctx mint name [ Sort.bitvec w ] (Sort.bitvec w) [ x ]
;;

let binop ctx mint op x y =
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
  build_app ctx mint name [ Sort.bitvec wx; Sort.bitvec wx ] result [ x; y ]
;;

let concat ctx mint hi lo =
  let w1 = width_exn hi ~what:"concat" in
  let w2 = width_exn lo ~what:"concat" in
  let name = Printf.sprintf "%sconcat|%d|%d" marker w1 w2 in
  build_app
    ctx
    mint
    name
    [ Sort.bitvec w1; Sort.bitvec w2 ]
    (Sort.bitvec (w1 + w2))
    [ hi; lo ]
;;

let extract ctx mint ~i ~j x =
  let w = width_exn x ~what:"extract" in
  if not (i >= j && j >= 0 && i < w)
  then
    raise
      (Term.Sort_error
         (Printf.sprintf "Bv.extract: indices [%d:%d] out of range for width %d" i j w));
  let name = Printf.sprintf "%sextract|%d|%d|%d" marker i j w in
  build_app ctx mint name [ Sort.bitvec w ] (Sort.bitvec (i - j + 1)) [ x ]
;;

let zero_extend ctx mint ~n x =
  let w = width_exn x ~what:"zero_extend" in
  if n < 0 then raise (Term.Sort_error "Bv.zero_extend: negative extension");
  let name = Printf.sprintf "%szero_extend|%d|%d" marker n w in
  build_app ctx mint name [ Sort.bitvec w ] (Sort.bitvec (w + n)) [ x ]
;;

let sign_extend ctx mint ~n x =
  let w = width_exn x ~what:"sign_extend" in
  if n < 0 then raise (Term.Sort_error "Bv.sign_extend: negative extension");
  let name = Printf.sprintf "%ssign_extend|%d|%d" marker n w in
  build_app ctx mint name [ Sort.bitvec w ] (Sort.bitvec (w + n)) [ x ]
;;
