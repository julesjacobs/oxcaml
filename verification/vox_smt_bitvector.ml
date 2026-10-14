module Term = Vox_smt_term
module Z = Vox_smt_integer.Z

type gate =
  | And of int * int
  | All of int array
  | Xor of int * int
  | Ite of int * int * int

type t =
  { terms : Term.t;
    fresh : unit -> int;
    clause : int array -> unit;
    boolean : Term.term -> int;
    int_bridge : Term.term -> int array -> unit;
    poll : unit -> unit;
    cache : (Term.term, int array) Hashtbl.t;
    gates : (gate, int) Hashtbl.t;
    divisions : (Term.term * Term.term, int array * int array) Hashtbl.t;
    mutable truth : int option
  }

exception Unsupported of string

let create ?(int_bridge = fun _ _ -> raise (Unsupported "int2bv bridge"))
    ?(poll = fun () -> ()) ~terms ~fresh ~clause ~boolean () =
  { terms;
    fresh;
    clause;
    boolean;
    int_bridge;
    poll;
    cache = Hashtbl.create 128;
    gates = Hashtbl.create 1024;
    divisions = Hashtbl.create 16;
    truth = None
  }

let truth t =
  match t.truth with
  | Some lit -> lit
  | None ->
    let lit = t.fresh () in
    t.clause [| lit |];
    t.truth <- Some lit;
    lit

let falsity t = -truth t

let gate t key clauses =
  match Hashtbl.find_opt t.gates key with
  | Some lit -> lit
  | None ->
    let lit = t.fresh () in
    List.iter t.clause (clauses lit);
    Hashtbl.add t.gates key lit;
    lit

let and_ t x y =
  t.poll ();
  let yes = truth t in
  if x = y || y = yes
  then x
  else if x = yes
  then y
  else if x = -y || x = -yes || y = -yes
  then -yes
  else
    let x, y = if x < y then x, y else y, x in
    gate t (And (x, y)) (fun z -> [[| -z; x |]; [| -z; y |]; [| z; -x; -y |]])

let or_ t x y = -and_ t (-x) (-y)

let xor t x y =
  t.poll ();
  let yes = truth t in
  if x = y
  then -yes
  else if x = -y
  then yes
  else if x = yes
  then -y
  else if y = yes
  then -x
  else if x = -yes
  then y
  else if y = -yes
  then x
  else
    let negative = x < 0 <> (y < 0) in
    let x, y = abs x, abs y in
    let x, y = if x < y then x, y else y, x in
    let z =
      gate t
        (Xor (x, y))
        (fun z ->
          [[| -z; x; y |]; [| -z; -x; -y |]; [| z; -x; y |]; [| z; x; -y |]])
    in
    if negative then -z else z

let ite t c x y =
  t.poll ();
  let yes = truth t in
  if c = yes || x = y
  then x
  else if c = -yes
  then y
  else if x = yes
  then or_ t c y
  else if x = -yes
  then and_ t (-c) y
  else if y = yes
  then or_ t (-c) x
  else if y = -yes
  then and_ t c x
  else if x = -y
  then -xor t c x
  else
    let c, x, y = if c > 0 then c, x, y else -c, y, x in
    gate t
      (Ite (c, x, y))
      (fun z ->
        [[| -c; -z; x |]; [| -c; z; -x |]; [| c; -z; y |]; [| c; z; -y |]])

let same_width a b =
  if Array.length a <> Array.length b
  then invalid_arg "SMT bitvector operand widths"

let choose t c a b =
  same_width a b;
  Array.map2 (ite t c) a b

let all t bits =
  let yes = truth t in
  let bits = Array.copy bits in
  Array.sort (fun x y ->
    t.poll ();
    let order = Int.compare (abs x) (abs y) in
    if order = 0 then Int.compare x y else order) bits;
  let length = ref 0 and previous = ref 0 and contradictory = ref false in
  Array.iter (fun bit ->
    t.poll ();
    if bit = -yes || bit = - !previous then contradictory := true;
    if bit <> yes && bit <> !previous then begin
      bits.(!length) <- bit;
      incr length
    end;
    previous := bit) bits;
  if !contradictory then -yes
  else if !length = 0 then yes
  else if !length = 1 then bits.(0)
  else if !length = 2 then and_ t bits.(0) bits.(1)
  else
    let bits = Array.sub bits 0 !length in
    gate t (All bits) (fun r ->
      let clauses = ref [] in
      let consequence = Array.make (!length + 1) r in
      Array.iteri (fun i bit ->
        t.poll ();
        consequence.(i + 1) <- -bit;
        clauses := [|-r; bit|] :: !clauses) bits;
      consequence :: !clauses)

let any t bits = -all t (Array.map ( ~- ) bits)

let equal t a b =
  same_width a b;
  all t (Array.map2 (fun x y -> -xor t x y) a b)

let add t a b carry =
  same_width a b;
  let carry = ref carry in
  Array.mapi
    (fun i x ->
      let y = b.(i) in
      let different = xor t x y in
      let bit = xor t different !carry in
      if i + 1 < Array.length a then carry := ite t different !carry x;
      bit)
    a

let add_plain t a b = add t a b (falsity t)

let sub t a b = add t a (Array.map ( ~- ) b) (truth t)

let negate t a =
  add t (Array.map ( ~- ) a)
    (Array.make (Array.length a) (falsity t))
    (truth t)

let unsigned_less t a b =
  same_width a b;
  let less = ref (falsity t) in
  Array.iteri (fun i x -> less := ite t (xor t x b.(i)) b.(i) !less) a;
  !less

let signed_less t a b =
  same_width a b;
  let last = Array.length a - 1 in
  ite t (xor t a.(last) b.(last)) a.(last) (unsigned_less t a b)

let multiply t a b =
  same_width a b;
  let width = Array.length a in
  let zero = falsity t in
  let cost xs =
    Array.fold_left (fun n x -> n + if x = zero then 0 else 1) 0 xs
  in
  let a, b = if cost a < cost b then b, a else a, b in
  let product = ref (Array.make width zero) in
  Array.iteri
    (fun shift bit ->
      t.poll ();
      if bit <> zero
      then
        let row =
          Array.init width (fun i ->
              if i < shift then zero else and_ t bit a.(i - shift))
        in
        product := add_plain t !product row)
    b;
  !product

let shift t kind a amount =
  let width = Array.length a in
  let zero = falsity t in
  let fill = if kind = "bvashr" then a.(width - 1) else zero in
  let result = ref a in
  let distance = ref 1 in
  let high = ref zero in
  Array.iter
    (fun bit ->
      t.poll ();
      if !distance >= width
      then high := or_ t !high bit
      else
        let previous = !result in
        result
          := Array.mapi
               (fun i current ->
                 let index =
                   if kind = "bvshl" then i - !distance else i + !distance
                 in
                 let shifted =
                   if index < 0 || index >= width
                   then fill
                   else previous.(index)
                 in
                 ite t bit shifted current)
               previous;
        distance := min width (2 * !distance))
    amount;
  Array.map (fun bit -> ite t !high fill bit) !result

let subtract_with_carry t a b =
  same_width a b;
  let carry = ref (truth t) in
  let result = Array.mapi (fun i x ->
    let different = xor t x (-b.(i)) in
    let bit = xor t different !carry in
    carry := ite t different !carry x;
    bit) a in
  result, !carry

let rec divide t a b =
  same_width a b;
  let width = Array.length a in
  let zero = falsity t in
  let used = ref width in
  while !used > 1 && a.(!used - 1) = zero do
    t.poll ();
    decr used
  done;
  if !used < width then begin
    let low_a = Array.sub a 0 !used and low_b = Array.sub b 0 !used in
    let high_b = any t (Array.sub b !used (width - !used)) in
    let quotient, remainder = divide t low_a low_b in
    let quotient = choose t high_b (Array.make !used zero) quotient in
    let remainder = choose t high_b low_a remainder in
    let nonzero = any t b in
    Array.append quotient (Array.make (width - !used) (-nonzero)),
    Array.append remainder (Array.make (width - !used) zero)
  end else
  let power = ref (-1) in
  let simple = ref true in
  Array.iteri
    (fun i bit ->
      if bit = -zero && !power < 0
      then power := i
      else if bit <> zero
      then simple := false)
    b;
  if !simple
  then
    if !power < 0
    then Array.make width (-zero), a
    else
      ( Array.init width (fun i ->
            if i + !power < width then a.(i + !power) else zero),
        Array.init width (fun i -> if i < !power then a.(i) else zero) )
  else
    let divisor = Array.append b [| zero |] in
    let remainder = ref (Array.make (width + 1) zero) in
    let quotient = Array.make width zero in
    for i = width - 1 downto 0 do
      t.poll ();
      let extended =
        Array.init (width + 1) (fun j ->
            if j = 0 then a.(i) else !remainder.(j - 1))
      in
      let difference, subtract = subtract_with_carry t extended divisor in
      quotient.(i) <- subtract;
      remainder := choose t subtract difference extended
    done;
    let remainder = Array.sub !remainder 0 width in
    let nonzero = any t b in
    let bounded = unsigned_less t remainder b in
    if bounded <> truth t then t.clause [|-nonzero; bounded|];
    if nonzero <> truth t then
      Array.iteri (fun i bit ->
        t.poll ();
        t.clause [|nonzero; bit|];
        t.clause [|nonzero; -remainder.(i); a.(i)|];
        t.clause [|nonzero; remainder.(i); -a.(i)|]) quotient;
    quotient, remainder

let constant t width value =
  let value = ref value in
  let two = Z.of_int 2 in
  Array.init width (fun _ ->
      let q, r = Z.div_rem ~poll:t.poll !value two in
      value := q;
      if Z.is_zero r then falsity t else truth t)

let arity name args n =
  if Array.length args <> n then invalid_arg ("SMT bitvector arity: " ^ name)

let index value =
  match int_of_string_opt value with
  | Some n when n >= 0 -> n
  | _ -> invalid_arg "SMT bitvector index"

let rec bits t term =
  match Hashtbl.find_opt t.cache term with
  | Some bits -> bits
  | None ->
    t.poll ();
    let width =
      match Term.sort t.terms term with
      | Term.Bitvec width when width > 0 -> width
      | _ -> invalid_arg "SMT bitvector expression required"
    in
    let result =
      match Term.Unsafe.node t.terms term with
      | Term.Bits (_, value) -> constant t width value
      | Term.Apply (symbol, args) -> (
        match (Term.Unsafe.signature t.terms symbol).kind with
        | Term.Uninterpreted | Term.Selector _ ->
          Array.init width (fun _ -> t.fresh ())
        | Term.Builtin name -> operation t width name args
        | _ -> raise (Unsupported "bitvector term"))
      | _ -> invalid_arg "SMT bitvector sort invariant"
    in
    if Array.length result <> width
    then invalid_arg "SMT bitvector result width";
    Hashtbl.add t.cache term result;
    result

and operation t width name args =
  let unary f =
    arity name args 1;
    f (bits t args.(0))
  in
  let binary f =
    arity name args 2;
    f (bits t args.(0)) (bits t args.(1))
  in
  let bitwise f =
    binary (fun a b ->
        same_width a b;
        Array.map2 f a b)
  in
  match name with
  | "bvnot" -> unary (Array.map ( ~- ))
  | "bvneg" -> unary (negate t)
  | "bvand" -> bitwise (and_ t)
  | "bvor" -> bitwise (or_ t)
  | "bvxor" -> bitwise (xor t)
  | "bvnand" -> bitwise (fun a b -> -and_ t a b)
  | "bvnor" -> bitwise (fun a b -> -or_ t a b)
  | "bvxnor" -> bitwise (fun a b -> -xor t a b)
  | "bvadd" -> binary (add_plain t)
  | "bvsub" -> binary (sub t)
  | "bvmul" -> binary (multiply t)
  | "bvshl" | "bvlshr" | "bvashr" ->
    binary (fun a b ->
        same_width a b;
        shift t name a b)
  | "bvudiv" | "bvurem" ->
    arity name args 2;
    let quotient, remainder = unsigned_division t args.(0) args.(1) in
    if name = "bvudiv" then quotient else remainder
  | "bvsdiv" | "bvsrem" | "bvsmod" ->
    binary (fun a b ->
        same_width a b;
        let sa = a.(Array.length a - 1) in
        let sb = b.(Array.length b - 1) in
        let aa = choose t sa (negate t a) a in
        let bb = choose t sb (negate t b) b in
        let quotient, remainder = divide t aa bb in
        if name = "bvsdiv"
        then choose t (xor t sa sb) (negate t quotient) quotient
        else
          let remainder = choose t sa (negate t remainder) remainder in
          if name = "bvsrem"
          then remainder
          else
            let adjust = and_ t (any t remainder) (xor t sa sb) in
            choose t adjust (add_plain t remainder b) remainder)
  | "bvcomp" -> binary (fun a b -> [| equal t a b |])
  | "bvredand" -> unary (fun a -> [| all t a |])
  | "bvredor" -> unary (fun a -> [| any t a |])
  | "concat" -> binary (fun a b -> Array.append b a)
  | "ite" ->
    arity name args 3;
    choose t (t.boolean args.(0)) (bits t args.(1)) (bits t args.(2))
  | _ -> (
    match String.split_on_char ':' name with
    | ["extract"; high; low] ->
      let high = index high and low = index low in
      unary (fun a ->
          if high < low || high >= Array.length a
          then invalid_arg "SMT bitvector extract range";
          Array.sub a low (high - low + 1))
    | [(("zero_extend" | "sign_extend") as op); count] ->
      let count = index count in
      unary (fun a ->
          let fill =
            if op = "sign_extend" then a.(Array.length a - 1) else falsity t
          in
          Array.append a (Array.make count fill))
    | ["repeat"; count] ->
      let count = index count in
      if count = 0 then invalid_arg "SMT bitvector repeat count";
      unary (fun a ->
          Array.init
            (count * Array.length a)
            (fun i -> a.(i mod Array.length a)))
    | [(("rotate_left" | "rotate_right") as op); count] ->
      let count = index count in
      unary (fun a ->
          let width = Array.length a in
          let count = count mod width in
          Array.init width (fun i ->
              let source =
                if op = "rotate_left" then i + width - count else i + count
              in
              a.(source mod width)))
    | ["int2bv"] | ["int2bv"; _] | ["int_to_bv"] | ["int_to_bv"; _] -> (
      arity name args 1;
      if Term.sort t.terms args.(0) <> Term.Int
      then invalid_arg "SMT int2bv source sort";
      match Term.Unsafe.node t.terms args.(0) with
      | Term.Integer value -> constant t width value
      | _ ->
        let result = Array.init width (fun _ -> t.fresh ()) in
        t.int_bridge args.(0) result;
        result)
    | _ -> raise (Unsupported ("bitvector operator " ^ name)))

and unsigned_division t a b =
  match Hashtbl.find_opt t.divisions (a, b) with
  | Some result -> result
  | None ->
    let shifted_one = match Term.Unsafe.node t.terms b with
      | Term.Apply (symbol, [|value; amount|])
          when (Term.Unsafe.signature t.terms symbol).kind
               = Term.Builtin "bvshl" ->
        (match Term.Unsafe.node t.terms value with
        | Term.Bits (_, value) when Z.equal value Z.one -> Some amount
        | _ -> None)
      | _ -> None in
    let result = match shifted_one with
      | None -> divide t (bits t a) (bits t b)
      | Some amount ->
        let numerator = bits t a and amount = bits t amount in
        let width = Array.length numerator in
        let ones = Array.make width (truth t) in
        let valid = unsigned_less t amount (constant t width (Z.of_int width)) in
        let quotient = choose t valid
            (shift t "bvlshr" numerator amount) ones in
        let high = shift t "bvshl" ones amount in
        let remainder = Array.map2 (fun bit high -> and_ t bit (-high))
            numerator high in
        quotient, remainder in
    Hashtbl.add t.divisions (a, b) result;
    result

let minimum t a =
  let last = Array.length a - 1 in
  and_ t a.(last) (-any t (Array.sub a 0 last))

let overflow t name a b =
  same_width a b;
  let width = Array.length a in
  let sa = a.(width - 1) and sb = b.(width - 1) in
  match name with
  | "bvuaddo" ->
    let carry = ref (falsity t) in
    Array.iteri (fun i x -> carry := ite t (xor t x b.(i)) !carry x) a;
    !carry
  | "bvsaddo" ->
    let sum = add_plain t a b in
    and_ t (-xor t sa sb) (xor t sa sum.(width - 1))
  | "bvusubo" -> unsigned_less t a b
  | "bvssubo" ->
    let difference = sub t a b in
    and_ t (xor t sa sb) (xor t sa difference.(width - 1))
  | "bvsdivo" -> and_ t (minimum t a) (all t b)
  | "bvumulo" | "bvsmulo" ->
    let signed = name = "bvsmulo" in
    let extend bits sign =
      Array.append bits (Array.make width (if signed then sign else falsity t))
    in
    let product = multiply t (extend a sa) (extend b sb) in
    let high = Array.sub product width width in
    if signed
    then any t (Array.map (xor t product.(width - 1)) high)
    else any t high
  | _ -> invalid_arg "SMT bitvector overflow predicate"

let at_most_one t a =
  let rec count start length =
    t.poll ();
    if length = 1 then a.(start), falsity t else
      let middle = length / 2 in
      let left, left_multiple = count start middle in
      let right, right_multiple = count (start + middle) (length - middle) in
      or_ t left right,
      or_ t (or_ t left_multiple right_multiple) (and_ t left right) in
  -snd (count 0 (Array.length a))

let at_most_one_argument t sort expression =
  let same_sort term = Term.sort t.terms term = sort in
  let decrements x y =
    same_sort x && same_sort y &&
    match Term.Unsafe.node t.terms y with
    | Term.Apply (symbol, [|value; one|])
        when value = x && same_sort one &&
             (Term.Unsafe.signature t.terms symbol).kind
             = Term.Builtin "bvsub" ->
      (match Term.Unsafe.node t.terms one with
      | Term.Bits (_, value) -> Z.equal value Z.one
      | _ -> false)
    | _ -> false in
  if not (same_sort expression) then None else
  match Term.Unsafe.node t.terms expression with
  | Term.Apply (symbol, [|left; right|])
      when (Term.Unsafe.signature t.terms symbol).kind
           = Term.Builtin "bvand" ->
    if decrements left right then Some left
    else if decrements right left then Some right else None
  | _ -> None

let predicate t name args =
  match name with
  | "bvnego" ->
    arity name args 1;
    Some (minimum t (bits t args.(0)))
  | "bvuaddo" | "bvsaddo" | "bvusubo" | "bvssubo" | "bvumulo" | "bvsmulo"
  | "bvsdivo" ->
    arity name args 2;
    Some (overflow t name (bits t args.(0)) (bits t args.(1)))
  | "=" | "distinct" | "bvult" | "bvule" | "bvugt" | "bvuge" | "bvslt"
  | "bvsle" | "bvsgt" | "bvsge" ->
    arity name args 2;
    let one_bit =
      if name <> "=" && name <> "distinct" then None else
        match Term.Unsafe.node t.terms args.(0),
              Term.Unsafe.node t.terms args.(1) with
        | Term.Bits (width, zero), _ when Z.is_zero zero ->
          at_most_one_argument t (Term.Bitvec width) args.(1)
        | _, Term.Bits (width, zero) when Z.is_zero zero ->
          at_most_one_argument t (Term.Bitvec width) args.(0)
        | _ -> None in
    (match one_bit with
    | Some argument ->
      let result = at_most_one t (bits t argument) in
      (match Term.Unsafe.node t.terms argument with
      | Term.Apply (symbol, [|left; right|])
          when (Term.Unsafe.signature t.terms symbol).kind
               = Term.Builtin "bvadd" ->
        let left = bits t left and right = bits t right in
        let left_single = at_most_one t left
        and right_single = at_most_one t right in
        (* Distinct nonzero powers of two sum to two set bits. *)
        t.clause [|-result; -left_single; -right_single;
          -any t left; -any t right; equal t left right|]
      | _ -> ());
      Some (if name = "=" then result else -result)
    | None ->
    let a = bits t args.(0) and b = bits t args.(1) in
    let result =
      match name with
      | "=" -> equal t a b
      | "distinct" -> -equal t a b
      | "bvult" -> unsigned_less t a b
      | "bvule" -> -unsigned_less t b a
      | "bvugt" -> unsigned_less t b a
      | "bvuge" -> -unsigned_less t a b
      | "bvslt" -> signed_less t a b
      | "bvsle" -> -signed_less t b a
      | "bvsgt" -> signed_less t b a
      | _ -> -signed_less t a b
    in
    Some result)
  | _ -> None
