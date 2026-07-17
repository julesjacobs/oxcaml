open Oxsmt_core

exception Eval_error of string

(* Project a term's arbitrary-precision constant/coefficient ([Bigint.t], core-bignum W2)
   to native [int]; a value exceeding int63 raises (fail-closed, never truncates). *)
let int_of_big b =
  match Bigint.to_int_opt b with
  | Some i -> i
  | None -> raise (Eval_error "integer literal/coefficient exceeds native int range")
;;

(* Overflow-guarded integer arithmetic: raise, never wrap (I8 spirit). *)
let add_ovf a b =
  let s = a + b in
  if (a >= 0 && b >= 0 && s < 0) || (a < 0 && b < 0 && s >= 0)
  then raise (Eval_error "integer overflow (addition)");
  s
;;

let neg_ovf a =
  if a = min_int then raise (Eval_error "integer overflow (negation of min_int)");
  -a
;;

let sub_ovf a b = add_ovf a (neg_ovf b)

let mul_ovf a b =
  if a = 0 || b = 0
  then 0
  else (
    let p = a * b in
    if p / a <> b || (a = -1 && b = min_int) || (b = -1 && a = min_int)
    then raise (Eval_error "integer overflow (multiplication)");
    p)
;;

(* Euclidean division/remainder (ADR-0003 Decision 5): 0 <= r < |d|, x = d*q + r. Derived
   from OCaml's truncated [/]/[mod] (remainder carries the dividend's sign) to avoid any
   overflowing intermediate: no [x - r] and no explicit [q] in the [mod] path. The one
   genuinely non-representable case, [min_int / -1] (whose true quotient -min_int
   overflows), raises rather than wrapping — an abstain (exit 2), never a wrong accept
   (codex E1). [d = min_int] also abstains: [abs min_int] is unrepresentable. *)

let guard_divisor d =
  if d = 0 then raise (Eval_error "division by zero");
  if d = min_int then raise (Eval_error "integer overflow (divisor = min_int)")
;;

(* Euclidean remainder alone — total for every representable [d <> 0, min_int]; [r] is
   always in [0, |d|) and its computation cannot overflow (we only ever add |d| to a
   strictly-negative truncated remainder, landing in (0, |d|)). *)
let euclid_rem x d =
  guard_divisor d;
  let tr = x mod d in
  if tr >= 0 then tr else tr + abs d
;;

(* Euclidean quotient. Guards [min_int / -1] before the hardware-overflowing [x / d]; the
   +/-1 truncated-to-euclidean adjustment is itself overflow-checked (abstain, never
   wrap). *)
let euclid_quot x d =
  guard_divisor d;
  if x = min_int && d = -1 then raise (Eval_error "integer overflow (min_int / -1)");
  let tq = x / d in
  let tr = x mod d in
  if tr >= 0 then tq else if d > 0 then sub_ovf tq 1 else add_ovf tq 1
;;

let as_int = function
  | Value.Int n -> n
  | v -> raise (Eval_error ("expected Int, got " ^ Value.to_string v))
;;

let as_real = function
  | Value.Real q -> q
  | v -> raise (Eval_error ("expected Real, got " ^ Value.to_string v))
;;

let as_bool = function
  | Value.Bool b -> b
  | v -> raise (Eval_error ("expected Bool, got " ^ Value.to_string v))
;;

(* Arbitrary-width bitvector arithmetic. This is independent of the solver's bit-blast
   evaluator: values are canonical unsigned Bigints, and the definitions below are the
   SMT-LIB denotations directly. Model ingestion is stricter than operation semantics:
   a model value outside [0,2^w) is rejected by [Eval_model], while operations whose
   specified result wraps (add/mul/etc.) reduce modulo [2^w] here. *)
let pow2_cache : (int, Bigint.t) Hashtbl.t = Hashtbl.create 16

let pow2 width =
  if width < 0 then raise (Eval_error "negative bitvector exponent");
  match Hashtbl.find_opt pow2_cache width with
  | Some value -> value
  | None ->
    let rec loop acc base n =
      if n = 0
      then acc
      else
        loop
          (if n land 1 = 1 then Bigint.mul acc base else acc)
          (if n = 1 then base else Bigint.mul base base)
          (n lsr 1)
    in
    let value = loop Bigint.one (Bigint.of_int 2) width in
    Hashtbl.replace pow2_cache width value;
    value
;;

let as_bv = function
  | Value.BitVec { width; bits } ->
    if width < 1 then raise (Eval_error "bitvector value has a non-positive width");
    if Bigint.sign bits < 0 || Bigint.compare bits (pow2 width) >= 0
    then raise (Eval_error "bitvector value is outside its declared width");
    width, bits
  | v -> raise (Eval_error ("expected BitVec, got " ^ Value.to_string v))
;;

let add_nonnegative what left right =
  if left < 0 || right < 0 || left > max_int - right
  then raise (Eval_error (what ^ ": native integer overflow"));
  left + right
;;

let mul_positive what left right =
  if left < 1 || right < 1 || left > max_int / right
  then raise (Eval_error (what ^ ": native integer overflow"));
  left * right
;;

let ensure_value_sort (sort : Sort.t) value =
  let matches =
    match sort, value with
    | Sort.Bool, Value.Bool _ -> true
    | Sort.Int _, Value.Int _ -> true
    | Sort.Real, Value.Real _ -> true
    | Sort.Uninterpreted expected, Value.Uninterp (actual, _) ->
      Sort.equal (Sort.uninterpreted expected) actual
    | Sort.BitVec expected, Value.BitVec _ ->
      let actual, _ = as_bv value in
      expected = actual
    | ( ( Sort.Bool
        | Sort.Int _
        | Sort.Real
        | Sort.Uninterpreted _
        | Sort.Datatype _
        | Sort.Array _
        | Sort.BitVec _ )
      , _ ) -> false
  in
  if not matches
  then raise (Eval_error ("value does not match term sort: " ^ Value.to_string value))
;;

let reduce_bv bits width =
  if width < 1 then raise (Eval_error "bitvector width must be positive");
  let modulus = pow2 width in
  let remainder = snd (Bigint.divmod bits modulus) in
  if Bigint.sign remainder < 0 then Bigint.add remainder modulus else remainder
;;

let bv width bits = Value.BitVec { width; bits = reduce_bv bits width }
let bv_max width = Bigint.sub (pow2 width) Bigint.one

let same_width left right =
  let wl, left = as_bv left
  and wr, right = as_bv right in
  if wl <> wr
  then raise (Eval_error (Printf.sprintf "bitvector width mismatch (%d versus %d)" wl wr));
  wl, left, right
;;

let bits_lsb value width =
  let two = Bigint.of_int 2 in
  let current = ref value in
  Array.init width (fun _ ->
    let quotient, remainder = Bigint.divmod !current two in
    current := quotient;
    not (Bigint.is_zero remainder))
;;

let bigint_of_bits_lsb bits =
  let two = Bigint.of_int 2 in
  let result = ref Bigint.zero in
  for i = Array.length bits - 1 downto 0 do
    result
    := Bigint.add (Bigint.mul !result two) (if bits.(i) then Bigint.one else Bigint.zero)
  done;
  !result
;;

let bitwise2 op left right =
  let width, left, right = same_width left right in
  let left = bits_lsb left width
  and right = bits_lsb right width in
  bv width (bigint_of_bits_lsb (Array.init width (fun i -> op left.(i) right.(i))))
;;

let signed bits width =
  if Bigint.compare bits (pow2 (width - 1)) >= 0
  then Bigint.sub bits (pow2 width)
  else bits
;;

let shift_amount bits width =
  match Bigint.to_int_opt bits with
  | Some amount when amount < width -> Some amount
  | Some _ | None -> None
;;

let logical_shift ~left value amount =
  let width, bits = as_bv value in
  match shift_amount amount width with
  | None -> bv width Bigint.zero
  | Some amount ->
    if left
    then bv width (Bigint.mul bits (pow2 amount))
    else bv width (fst (Bigint.divmod bits (pow2 amount)))
;;

let arithmetic_shift_right value amount =
  let width, bits = as_bv value in
  let sign = Bigint.compare bits (pow2 (width - 1)) >= 0 in
  match shift_amount amount width with
  | None -> bv width (if sign then bv_max width else Bigint.zero)
  | Some amount ->
    let input = bits_lsb bits width in
    let output =
      Array.init width (fun index ->
        let source = add_nonnegative "arithmetic shift index" index amount in
        if source < width then input.(source) else sign)
    in
    bv width (bigint_of_bits_lsb output)
;;

let rotate ~left value amount =
  let width, bits = as_bv value in
  let amount = amount mod width in
  let input = bits_lsb bits width in
  let output =
    Array.init width (fun index ->
      if left
      then input.(if index >= amount then index - amount else width - (amount - index))
      else (
        let source = add_nonnegative "rotate index" index amount in
        input.(if source >= width then source - width else source)))
  in
  bv width (bigint_of_bits_lsb output)
;;

let signed_div left right =
  let width, left, right = same_width left right in
  let left = signed left width
  and right = signed right width in
  if Bigint.is_zero right
  then bv width (if Bigint.sign left < 0 then Bigint.one else bv_max width)
  else bv width (fst (Bigint.divmod left right))
;;

let signed_rem left right =
  let width, left, right = same_width left right in
  let signed_left = signed left width
  and signed_right = signed right width in
  if Bigint.is_zero signed_right
  then bv width left
  else bv width (snd (Bigint.divmod signed_left signed_right))
;;

let signed_mod left right =
  let width, left, right = same_width left right in
  let signed_left = signed left width
  and signed_right = signed right width in
  if Bigint.is_zero signed_right
  then bv width left
  else (
    let magnitude =
      snd (Bigint.divmod (Bigint.abs signed_left) (Bigint.abs signed_right))
    in
    let result =
      if Bigint.is_zero magnitude
      then Bigint.zero
      else if Bigint.sign signed_left >= 0 && Bigint.sign signed_right >= 0
      then magnitude
      else if Bigint.sign signed_left < 0 && Bigint.sign signed_right >= 0
      then Bigint.sub signed_right magnitude
      else if Bigint.sign signed_left >= 0 && Bigint.sign signed_right < 0
      then Bigint.add signed_right magnitude
      else Bigint.neg magnitude
    in
    bv width result)
;;

let eval_general ~consts ~funs (root : Term.t) : Value.t =
  let rec go (t : Term.t) : Value.t =
    let value =
      match t.node with
      | Term.Bool_const b -> Value.Bool b
      | Term.Int_const n -> Value.Int (int_of_big n)
      | Term.Real_const q -> Value.Real (Value.Rational.of_term q)
      | Term.Not a -> Value.Bool (not (as_bool (go a)))
      | Term.And xs ->
        (* fold so every operand is forced (model errors stay loud past a false one) *)
        Value.Bool
          (Iarr.fold
             (fun acc x ->
               let b = as_bool (go x) in
               acc && b)
             true
             xs)
      | Term.Or xs ->
        Value.Bool
          (Iarr.fold
             (fun acc x ->
               let b = as_bool (go x) in
               acc || b)
             false
             xs)
      | Term.Ite (c, a, b) -> if as_bool (go c) then go a else go b
      | Term.Eq (a, b) -> Value.Bool (Value.equal (go a) (go b))
      | Term.Le arg ->
        (match arg.sort with
         | Sort.Int _ -> Value.Bool (as_int (go arg) <= 0)
         | Sort.Real ->
           Value.Bool (Value.Rational.compare (as_real (go arg)) Value.Rational.zero <= 0)
         | _ -> raise (Eval_error "Le argument is not numeric"))
      | Term.Arith { coeffs; const } ->
        let sum =
          Iarr.fold
            (fun acc (ti, ci) -> add_ovf acc (mul_ovf (int_of_big ci) (as_int (go ti))))
            (int_of_big const)
            coeffs
        in
        Value.Int sum
      | Term.Real_arith { coeffs; const } ->
        let sum =
          Iarr.fold
            (fun acc (ti, ci) ->
              Value.Rational.add
                acc
                (Value.Rational.mul (Value.Rational.of_term ci) (as_real (go ti))))
            (Value.Rational.of_term const)
            coeffs
        in
        Value.Real sum
      | Term.App (sym, args) ->
        (match Bv_term.view t with
         | Some view -> eval_bv view
         | None -> eval_app sym args)
    in
    ensure_value_sort t.sort value;
    value
  and eval_bv = function
    | Bv_term.Const { bits; width } -> bv width bits
    | Bv_term.Op { op; args } ->
      let one () = go (List.hd args) in
      let two () = go (List.hd args), go (List.nth args 1) in
      let unsigned_compare cmp () =
        let left, right = two () in
        let _, left, right = same_width left right in
        Value.Bool (cmp (Bigint.compare left right) 0)
      in
      let signed_compare cmp () =
        let left, right = two () in
        let width, left, right = same_width left right in
        Value.Bool (cmp (Bigint.compare (signed left width) (signed right width)) 0)
      in
      (match op with
       | Bv_term.Not ->
         let width, bits = as_bv (one ()) in
         bv width (Bigint.sub (bv_max width) bits)
       | Bv_term.And ->
         let left, right = two () in
         bitwise2 ( && ) left right
       | Bv_term.Or ->
         let left, right = two () in
         bitwise2 ( || ) left right
       | Bv_term.Xor ->
         let left, right = two () in
         bitwise2 ( <> ) left right
       | Bv_term.Nand ->
         let left, right = two () in
         let width, bits = as_bv (bitwise2 ( && ) left right) in
         bv width (Bigint.sub (bv_max width) bits)
       | Bv_term.Nor ->
         let left, right = two () in
         let width, bits = as_bv (bitwise2 ( || ) left right) in
         bv width (Bigint.sub (bv_max width) bits)
       | Bv_term.Xnor ->
         let left, right = two () in
         let width, bits = as_bv (bitwise2 ( <> ) left right) in
         bv width (Bigint.sub (bv_max width) bits)
       | Bv_term.Neg ->
         let width, bits = as_bv (one ()) in
         bv width (Bigint.neg bits)
       | Bv_term.Add ->
         let left, right = two () in
         let width, left, right = same_width left right in
         bv width (Bigint.add left right)
       | Bv_term.Sub ->
         let left, right = two () in
         let width, left, right = same_width left right in
         bv width (Bigint.sub left right)
       | Bv_term.Mul ->
         let left, right = two () in
         let width, left, right = same_width left right in
         bv width (Bigint.mul left right)
       | Bv_term.Udiv ->
         let left, right = two () in
         let width, left, right = same_width left right in
         if Bigint.is_zero right
         then bv width (bv_max width)
         else bv width (fst (Bigint.divmod left right))
       | Bv_term.Urem ->
         let left, right = two () in
         let width, left, right = same_width left right in
         if Bigint.is_zero right
         then bv width left
         else bv width (snd (Bigint.divmod left right))
       | Bv_term.Sdiv ->
         let left, right = two () in
         signed_div left right
       | Bv_term.Srem ->
         let left, right = two () in
         signed_rem left right
       | Bv_term.Smod ->
         let left, right = two () in
         signed_mod left right
       | Bv_term.Shl ->
         let value, amount = two () in
         let _, amount = as_bv amount in
         logical_shift ~left:true value amount
       | Bv_term.Lshr ->
         let value, amount = two () in
         let _, amount = as_bv amount in
         logical_shift ~left:false value amount
       | Bv_term.Ashr ->
         let value, amount = two () in
         let _, amount = as_bv amount in
         arithmetic_shift_right value amount
       | Bv_term.Ult -> unsigned_compare ( < ) ()
       | Bv_term.Ule -> unsigned_compare ( <= ) ()
       | Bv_term.Ugt -> unsigned_compare ( > ) ()
       | Bv_term.Uge -> unsigned_compare ( >= ) ()
       | Bv_term.Slt -> signed_compare ( < ) ()
       | Bv_term.Sle -> signed_compare ( <= ) ()
       | Bv_term.Sgt -> signed_compare ( > ) ()
       | Bv_term.Sge -> signed_compare ( >= ) ()
       | Bv_term.Comp ->
         let left, right = two () in
         let _, left, right = same_width left right in
         bv 1 (if Bigint.equal left right then Bigint.one else Bigint.zero)
       | Bv_term.Concat ->
         let high, low = two () in
         let high_width, high = as_bv high
         and low_width, low = as_bv low in
         let result_width = add_nonnegative "concat width" high_width low_width in
         bv result_width (Bigint.add (Bigint.mul high (pow2 low_width)) low)
       | Bv_term.Extract (high, low) ->
         let _, bits = as_bv (one ()) in
         let bits = fst (Bigint.divmod bits (pow2 low)) in
         bv (add_nonnegative "extract width" (high - low) 1) bits
       | Bv_term.Zero_extend amount ->
         let width, bits = as_bv (one ()) in
         bv (add_nonnegative "zero-extend width" width amount) bits
       | Bv_term.Sign_extend amount ->
         let width, bits = as_bv (one ()) in
         let result_width = add_nonnegative "sign-extend width" width amount in
         if Bigint.compare bits (pow2 (width - 1)) < 0
         then bv result_width bits
         else
           bv result_width (Bigint.add bits (Bigint.sub (pow2 result_width) (pow2 width)))
       | Bv_term.Rotate_left amount -> rotate ~left:true (one ()) amount
       | Bv_term.Rotate_right amount -> rotate ~left:false (one ()) amount
       | Bv_term.Repeat count ->
         let width, bits = as_bv (one ()) in
         let result = ref Bigint.zero in
         for _ = 1 to count do
           result := Bigint.add (Bigint.mul !result (pow2 width)) bits
         done;
         bv (mul_positive "repeat width" width count) !result)
  and eval_app sym args =
    let name = Symbol.name sym in
    match name, Iarr.length args with
    | "div", 2 ->
      Value.Int
        (euclid_quot (as_int (go (Iarr.get args 0))) (as_int (go (Iarr.get args 1))))
    | "mod", 2 ->
      Value.Int
        (euclid_rem (as_int (go (Iarr.get args 0))) (as_int (go (Iarr.get args 1))))
    | _, 0 ->
      (match consts sym with
       | Some v -> v
       | None -> raise (Eval_error ("model does not define constant " ^ name)))
    | _, _ ->
      (match funs sym with
       | None -> raise (Eval_error ("model does not define function " ^ name))
       | Some (table : Eval_model.fun_table) ->
         let argv = List.map go (Iarr.to_list args) in
         let matches (case_args, _) =
           List.length case_args = List.length argv
           && List.for_all2 Value.equal case_args argv
         in
         (match List.find_opt matches table.cases with
          | Some (_, res) -> res
          | None -> table.default))
  in
  go root
;;

let eval (model : Eval_model.t) (t : Term.t) : Value.t =
  eval_general
    ~consts:(Eval_model.lookup_const model)
    ~funs:(Eval_model.lookup_fun model)
    t
;;

let eval_term ~env (t : Term.t) : Value.t =
  eval_general ~consts:env ~funs:(fun _ -> None) t
;;

(* --- Failing-path rendering --------------------------------------------------------- *)

let head_and_children (t : Term.t) : string * Term.t list =
  match t.node with
  | Term.Bool_const b -> Bool.to_string b, []
  | Term.Int_const n -> Bigint.to_string n, []
  | Term.Real_const q -> Value.Rational.to_string (Value.Rational.of_term q), []
  | Term.Not a -> "(not _)", [ a ]
  | Term.And xs -> "(and ...)", Iarr.to_list xs
  | Term.Or xs -> "(or ...)", Iarr.to_list xs
  | Term.Ite (c, a, b) -> "(ite _ _ _)", [ c; a; b ]
  | Term.Eq (a, b) -> "(= _ _)", [ a; b ]
  | Term.Le a -> "(<= _ 0)", [ a ]
  | Term.Arith { coeffs; const } ->
    ( Printf.sprintf "(linear +%s ...)" (Bigint.to_string const)
    , List.map fst (Iarr.to_list coeffs) )
  | Term.Real_arith { coeffs; const } ->
    ( Printf.sprintf
        "(real-linear +%s ...)"
        (Value.Rational.to_string (Value.Rational.of_term const))
    , List.map fst (Iarr.to_list coeffs) )
  | Term.App (sym, args) ->
    let name = Symbol.name sym in
    let name =
      match Bv_term.view t with
      | Some (Bv_term.Const { bits; width }) ->
        Printf.sprintf "(_ bv%s %d)" (Bigint.to_string bits) width
      | Some (Bv_term.Op { op; _ }) -> Bv_term.op_name op
      | None -> name
    in
    ( (if Iarr.length args = 0 then name else Printf.sprintf "(%s ...)" name)
    , Iarr.to_list args )
;;

let explain (model : Eval_model.t) (t : Term.t) : string =
  let buf = Buffer.create 256 in
  let rec go depth indent (node : Term.t) =
    let head, children = head_and_children node in
    let vstr =
      match eval model node with
      | v -> Value.to_string v
      | exception Eval_error msg -> "<error: " ^ msg ^ ">"
    in
    Buffer.add_string buf (Printf.sprintf "%s%s => %s\n" indent head vstr);
    if depth > 0 then List.iter (go (depth - 1) (indent ^ "  ")) children
  in
  go 4 "" t;
  Buffer.contents buf
;;

type outcome =
  | Satisfies
  | Fails of
      { index : int
      ; trace : string
      }

(* Formula-completeness (UF-models ADR §4 R7). The emitted model must be FORMULA-complete:
   it must bind every symbol that SYNTACTICALLY appears in the assertions and supply a
   [(sort S k)] cardinality entry for every uninterpreted sort any subterm has. Two points
   are deliberate:
   - Syntactic, not semantic: a symbol under an UNTAKEN [ite] branch still counts, so this
     is stricter than lazy evaluation would be. Rejecting a model that omits it is
     fail-closed — a completeness loss (abstain), never a wrong [Satisfies].
   - Declared-but-UNUSED symbols/sorts are NOT required (that is signature-completeness,
     which the ADR rejects for the emitted contract). The reserved [div]/[mod] built-ins
     (name + arity 2) are not model-bound and are skipped, mirroring {!eval_app}. Missing
     data is a loud {!Eval_model.Malformed}. *)
let require_formula_complete (model : Eval_model.t) (assertions : Term.t list) : unit =
  let seen : (int, unit) Hashtbl.t = Hashtbl.create 256 in
  let missing = ref [] in
  let note m = if not (List.mem m !missing) then missing := m :: !missing in
  let check_sort (sort : Sort.t) =
    match sort with
    | Sort.Uninterpreted sym ->
      let name = Symbol.name sym in
      if Option.is_none (Eval_model.sort_card model name)
      then note (Printf.sprintf "cardinality entry for sort %s" name)
    (* No datatype-sorted model completeness obligation yet: the datatype theory emits no
       model values, so a datatype query never reaches [Sat]-with-model here. *)
    | Sort.Bool | Sort.Int _ | Sort.Real | Sort.Datatype _ | Sort.Array _ | Sort.BitVec _
      -> ()
  in
  let check_app sym arity =
    let name = Symbol.name sym in
    if not ((name = "div" || name = "mod") && arity = 2)
    then (
      let bound =
        if arity = 0
        then Option.is_some (Eval_model.lookup_const model sym)
        else Option.is_some (Eval_model.lookup_fun model sym)
      in
      if not bound then note (Printf.sprintf "binding for symbol %s" name))
  in
  let rec walk (t : Term.t) =
    if not (Hashtbl.mem seen t.tag)
    then (
      Hashtbl.add seen t.tag ();
      check_sort t.sort;
      match t.node with
      | Term.Bool_const _ | Term.Int_const _ | Term.Real_const _ -> ()
      | Term.Not a -> walk a
      | Term.And xs | Term.Or xs -> Iarr.iter walk xs
      | Term.Ite (c, a, b) ->
        walk c;
        walk a;
        walk b
      | Term.Eq (a, b) ->
        walk a;
        walk b
      | Term.Le a -> walk a
      | Term.Arith { coeffs; _ } -> Iarr.iter (fun (ti, _) -> walk ti) coeffs
      | Term.Real_arith { coeffs; _ } -> Iarr.iter (fun (ti, _) -> walk ti) coeffs
      | Term.App (sym, args) ->
        (match Bv_term.view t with
         | Some _ -> ()
         | None -> check_app sym (Iarr.length args));
        Iarr.iter walk args)
  in
  List.iter walk assertions;
  match List.rev !missing with
  | [] -> ()
  | ms ->
    raise
      (Eval_model.Malformed
         ("model is not formula-complete; missing " ^ String.concat ", " ms))
;;

let check (model : Eval_model.t) (assertions : Term.t list) : outcome =
  require_formula_complete model assertions;
  let rec loop i = function
    | [] -> Satisfies
    | t :: tl ->
      (match eval model t with
       | Value.Bool true -> loop (i + 1) tl
       | Value.Bool false -> Fails { index = i; trace = explain model t }
       | v ->
         raise (Eval_error ("assertion did not evaluate to Bool: " ^ Value.to_string v)))
  in
  loop 0 assertions
;;
