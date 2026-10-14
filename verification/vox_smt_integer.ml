let no_poll () = ()

module Z = struct
  type t = Small of int | Large of int * int array

  let bits = if Sys.int_size >= 63 then 30 else 15
  let base = 1 lsl bits
  let mask = base - 1
  let zero = Small 0
  let one = Small 1
  let minus_one = Small (-1)
  let of_int n = Small n

  let sign = function
    | Small n -> Int.compare n 0
    | Large (sign, _) -> sign

  let is_zero = function Small 0 -> true | _ -> false

  let normalize sign digits =
    let length = ref (Array.length digits) in
    while !length > 0 && digits.(!length - 1) = 0 do
      decr length
    done;
    if !length = 0
    then zero
    else
      let small = ref true and value = ref 0 in
      for i = !length - 1 downto 0 do
        if
          !value < min_int / base
          || (!value = min_int / base
             && digits.(i) > -(min_int mod base))
        then small := false;
        if !small then value := (!value * base) - digits.(i)
      done;
      if !small && (sign < 0 || !value <> min_int)
      then Small (if sign < 0 then !value else - !value)
      else
        Large
          (sign,
           if !length = Array.length digits
           then digits
           else Array.sub digits 0 !length)

  let magnitude = function
    | Large (_, digits) -> digits
    | Small 0 -> [||]
    | Small n ->
      let rec loop n digits =
        if n = 0
        then Array.of_list (List.rev digits)
        else loop (n / base) (abs (n mod base) :: digits)
      in
      loop n []

  let compare_magnitude a b =
    let c = Int.compare (Array.length a) (Array.length b) in
    if c <> 0
    then c
    else
      let i = ref (Array.length a - 1) in
      while !i >= 0 && a.(!i) = b.(!i) do
        decr i
      done;
      if !i < 0 then 0 else Int.compare a.(!i) b.(!i)

  let compare a b =
    match a, b with
    | Small a, Small b -> Int.compare a b
    | Small _, Large (sign, _) -> -sign
    | Large (sign, _), Small _ -> sign
    | Large (sa, a), Large (sb, b) ->
      if sa <> sb then Int.compare sa sb else sa * compare_magnitude a b

  let equal a b = compare a b = 0

  let minimum_magnitude = magnitude (Small min_int)

  let neg = function
    | Small n when n <> min_int -> Small (-n)
    | Large (sign, digits) ->
      if sign > 0 && compare_magnitude digits minimum_magnitude = 0
      then Small min_int
      else Large (-sign, digits)
    | Small _ -> Large (1, minimum_magnitude)

  let abs n = if sign n < 0 then neg n else n

  let add_magnitude ~poll a b =
    let na = Array.length a and nb = Array.length b in
    let n = max na nb in
    let result = Array.make (n + 1) 0 and carry = ref 0 in
    for i = 0 to n - 1 do
      poll ();
      let sum =
        (if i < na then a.(i) else 0)
        + (if i < nb then b.(i) else 0) + !carry
      in
      result.(i) <- sum land mask;
      carry := sum lsr bits
    done;
    result.(n) <- !carry;
    result

  let sub_magnitude ~poll a b =
    let result = Array.make (Array.length a) 0 and borrow = ref 0 in
    for i = 0 to Array.length a - 1 do
      poll ();
      let difference =
        a.(i) - (if i < Array.length b then b.(i) else 0) - !borrow
      in
      result.(i) <- difference land mask;
      borrow := if difference < 0 then 1 else 0
    done;
    result

  let add ?(poll = no_poll) a b =
    poll ();
    match a, b with
    | Small 0, _ -> b
    | _, Small 0 -> a
    | Small x, Small y
      when (y >= 0 && x <= max_int - y)
           || (y < 0 && x >= min_int - y) ->
      Small (x + y)
    | _ ->
      let sa = sign a and sb = sign b in
      let a = magnitude a and b = magnitude b in
      if sa = sb
      then normalize sa (add_magnitude ~poll a b)
      else
        let c = compare_magnitude a b in
        if c = 0
        then zero
        else if c > 0
        then normalize sa (sub_magnitude ~poll a b)
        else normalize sb (sub_magnitude ~poll b a)

  let sub ?(poll = no_poll) a b = add ~poll a (neg b)

  let mul ?(poll = no_poll) a b =
    poll ();
    match a, b with
    | Small 0, _ | _, Small 0 -> zero
    | Small 1, _ -> b
    | _, Small 1 -> a
    | Small (-1), _ -> neg b
    | _, Small (-1) -> neg a
    | Small x, Small y
      when x <> min_int && y <> min_int
           && Stdlib.abs x <= max_int / Stdlib.abs y ->
      Small (x * y)
    | _ ->
      let sign = sign a * sign b in
      let a = magnitude a and b = magnitude b in
      let na = Array.length a and nb = Array.length b in
      let result = Array.make (na + nb) 0 in
      for i = 0 to na - 1 do
        let carry = ref 0 in
        for j = 0 to nb - 1 do
          poll ();
          let product = (a.(i) * b.(j)) + result.(i + j) + !carry in
          result.(i + j) <- product land mask;
          carry := product lsr bits
        done;
        result.(i + nb) <- !carry
      done;
      normalize sign result

  let div_small ~poll digits divisor =
    let result = Array.make (Array.length digits) 0 and remainder = ref 0 in
    for i = Array.length digits - 1 downto 0 do
      poll ();
      let value = (!remainder * base) + digits.(i) in
      result.(i) <- value / divisor;
      remainder := value mod divisor
    done;
    result, !remainder

  let shift_left digits amount extra =
    let n = Array.length digits in
    let result = Array.make (n + extra) 0 and carry = ref 0 in
    for i = 0 to n - 1 do
      let value = (digits.(i) lsl amount) lor !carry in
      result.(i) <- value land mask;
      carry := value lsr bits
    done;
    if extra > 0 then result.(n) <- !carry;
    result

  let div_magnitude ~poll a b =
    let n = Array.length b in
    if n = 1
    then
      let quotient, remainder = div_small ~poll a b.(0) in
      quotient, [| remainder |]
    else
      let shift = ref 0 and top = ref b.(n - 1) in
      while !top < base / 2 do
        incr shift;
        top := !top lsl 1
      done;
      let v = shift_left b !shift 0 in
      let u = shift_left a !shift 1 in
      let m = Array.length a - n in
      let quotient = Array.make (m + 1) 0 in
      for j = m downto 0 do
        poll ();
        let numerator = (u.(j + n) * base) + u.(j + n - 1) in
        let estimate = ref (min mask (numerator / v.(n - 1))) in
        let remainder = ref (numerator - (!estimate * v.(n - 1))) in
        while
          !remainder < base
          && !estimate * v.(n - 2)
             > (!remainder * base) + u.(j + n - 2)
        do
          decr estimate;
          remainder := !remainder + v.(n - 1)
        done;
        let borrow = ref 0 in
        for i = 0 to n - 1 do
          poll ();
          let product = (!estimate * v.(i)) + !borrow in
          let difference = u.(j + i) - (product land mask) in
          u.(j + i) <- difference land mask;
          borrow := (product lsr bits) + if difference < 0 then 1 else 0
        done;
        let difference = u.(j + n) - !borrow in
        u.(j + n) <- difference land mask;
        if difference < 0
        then begin
          decr estimate;
          let carry = ref 0 in
          for i = 0 to n - 1 do
            poll ();
            let sum = u.(j + i) + v.(i) + !carry in
            u.(j + i) <- sum land mask;
            carry := sum lsr bits
          done;
          u.(j + n) <- (u.(j + n) + !carry) land mask
        end;
        quotient.(j) <- !estimate
      done;
      let remainder = Array.make n 0 and carry = ref 0 in
      let low_mask = (1 lsl !shift) - 1 in
      for i = n - 1 downto 0 do
        poll ();
        remainder.(i) <-
          (u.(i) lsr !shift) lor (!carry lsl (bits - !shift));
        carry := u.(i) land low_mask
      done;
      quotient, remainder

  let quot_rem ?(poll = no_poll) a b =
    poll ();
    if is_zero b then raise Division_by_zero;
    match a, b with
    | _, Small 1 -> a, zero
    | _, Small (-1) -> neg a, zero
    | Small x, Small y when x <> min_int || y <> -1 ->
      Small (x / y), Small (x mod y)
    | _ ->
      let sa = sign a and sb = sign b in
      let ad = magnitude a and bd = magnitude b in
      let c = compare_magnitude ad bd in
      if c < 0
      then zero, a
      else if c = 0
      then of_int (sa * sb), zero
      else
        let q, r = div_magnitude ~poll ad bd in
        normalize (sa * sb) q, normalize sa r

  let div_rem ?(poll = no_poll) a b =
    let q, r = quot_rem ~poll a b in
    if sign r >= 0
    then q, r
    else sub ~poll q (of_int (sign b)), add ~poll r (abs b)

  let floor_div ?(poll = no_poll) a b =
    let q, r = quot_rem ~poll a b in
    if not (is_zero r) && sign a <> sign b then sub ~poll q one else q

  let ceil_div ?(poll = no_poll) a b =
    let q, r = quot_rem ~poll a b in
    if not (is_zero r) && sign a = sign b then add ~poll q one else q

  let gcd ?(poll = no_poll) a b =
    match a, b with
    | Small x, Small y when x <> min_int && y <> min_int ->
      let rec loop a b =
        poll ();
        if b = 0 then Small a else loop b (a mod b)
      in
      loop (Int.abs x) (Int.abs y)
    | _ ->
      let rec loop a b =
        poll ();
        if is_zero b then a else loop b (snd (quot_rem ~poll a b))
      in
      loop (abs a) (abs b)

  let pow ?(poll = no_poll) a exponent =
    if exponent < 0 then invalid_arg "Vox_smt_integer.Z.pow";
    let rec loop result a n =
      poll ();
      if n = 0
      then result
      else
        let result = if n land 1 = 0 then result else mul ~poll result a in
        if n = 1 then result else loop result (mul ~poll a a) (n lsr 1)
    in
    loop one a exponent

  let of_string ?(poll = no_poll) text =
    let length = String.length text in
    if length = 0 then invalid_arg "Vox_smt_integer.Z.of_string";
    let negative = text.[0] = '-' in
    let first = if negative || text.[0] = '+' then 1 else 0 in
    if first = length then invalid_arg "Vox_smt_integer.Z.of_string";
    let result = ref zero in
    for i = first to length - 1 do
      let digit = Char.code text.[i] - Char.code '0' in
      if digit < 0 || digit > 9
      then invalid_arg "Vox_smt_integer.Z.of_string";
      result := add ~poll (mul ~poll !result (Small 10)) (Small digit)
    done;
    if negative then neg !result else !result

  let to_int_opt = function Small n -> Some n | Large _ -> None

  let log2 ?(poll = no_poll) n =
    poll ();
    if sign n <= 0 then zero else
    let digits = magnitude n in
    let length = Array.length digits in
    let top = ref digits.(length - 1) and width = ref 0 in
    while !top > 1 do
      top := !top lsr 1;
      incr width
    done;
    of_int ((length - 1) * bits + !width)

  let maximum_materialized_bits = 1_048_576

  let pow2 ?(poll = no_poll) exponent =
    poll ();
    if sign exponent < 0 then Some zero else
    match to_int_opt exponent with
    | Some exponent when exponent < maximum_materialized_bits ->
      let digits = Array.init (exponent / bits + 1) (fun index ->
        poll ();
        if index = exponent / bits then 1 lsl (exponent mod bits) else 0) in
      Some (normalize 1 digits)
    | _ -> None

  let piand ?(poll = no_poll) width left right =
    poll ();
    if sign width <= 0 || is_zero left || is_zero right then Some zero else
    let width = List.fold_left (fun width operand ->
      if sign operand < 0 then width else
      let bound = add ~poll (log2 ~poll operand) one in
      if compare bound width < 0 then bound else width)
        width [left; right] in
    match to_int_opt width with
    | Some width when width <= maximum_materialized_bits ->
      let digit_reader operand =
        let negative = sign operand < 0 in
        let digits = magnitude
            (if negative then sub ~poll (neg operand) one else operand) in
        fun index ->
          let digit = if index < Array.length digits then digits.(index)
            else 0 in
          if negative then digit lxor mask else digit in
      let left = digit_reader left and right = digit_reader right in
      let count = (width + bits - 1) / bits in
      let digits = Array.init count (fun index ->
        poll ();
        let digit = left index land right index in
        if index = count - 1 && width mod bits <> 0
        then digit land ((1 lsl (width mod bits)) - 1) else digit) in
      Some (normalize 1 digits)
    | _ -> None

  let to_string ?(poll = no_poll) = function
    | Small n -> string_of_int n
    | Large (sign, digits) ->
      let decimal, width = if bits = 30 then 1_000_000_000, 9 else 10_000, 4 in
      let rec loop digits parts =
        let quotient, remainder = div_small ~poll digits decimal in
        let length = ref (Array.length quotient) in
        while !length > 0 && quotient.(!length - 1) = 0 do
          decr length
        done;
        if !length = 0
        then string_of_int remainder :: parts
        else
          loop (Array.sub quotient 0 !length)
            (Printf.sprintf "%0*d" width remainder :: parts)
      in
      (if sign < 0 then "-" else "") ^ String.concat "" (loop digits [])
end

module Q = struct
  type t = { numerator : Z.t; denominator : Z.t }

  let small_values = Array.init 513 (fun n ->
    { numerator = Z.of_int (n - 256); denominator = Z.one })

  let of_z numerator =
    match numerator with
    | Z.Small n when n >= -256 && n <= 256 -> small_values.(n + 256)
    | _ -> { numerator; denominator = Z.one }
  let of_int n = of_z (Z.of_int n)
  let zero = of_z Z.zero
  let one = of_z Z.one
  let numerator q = q.numerator
  let denominator q = q.denominator
  let sign q = Z.sign q.numerator
  let is_zero q = Z.is_zero q.numerator
  let is_integer q = Z.equal q.denominator Z.one

  let exact_div ~poll a b =
    if Z.equal b Z.one then a else fst (Z.quot_rem ~poll a b)

  let make ?(poll = no_poll) numerator denominator =
    poll ();
    if Z.is_zero denominator then raise Division_by_zero;
    if Z.is_zero numerator
    then zero
    else
      let numerator, denominator =
        if Z.sign denominator < 0
        then Z.neg numerator, Z.neg denominator
        else numerator, denominator
      in
      if Z.equal denominator Z.one
      then of_z numerator
      else if Z.equal (Z.abs numerator) Z.one
      then { numerator; denominator }
      else
        let divisor = Z.gcd ~poll numerator denominator in
        { numerator = exact_div ~poll numerator divisor;
          denominator = exact_div ~poll denominator divisor
        }

  let equal a b =
    Z.equal a.numerator b.numerator && Z.equal a.denominator b.denominator

  let compare ?(poll = no_poll) a b =
    poll ();
    if Z.equal a.denominator b.denominator
    then Z.compare a.numerator b.numerator
    else
      let sa = sign a and sb = sign b in
      if sa <> sb
      then Int.compare sa sb
      else
        Z.compare
          (Z.mul ~poll a.numerator b.denominator)
          (Z.mul ~poll b.numerator a.denominator)

  let neg q =
    let numerator = Z.neg q.numerator in
    if is_integer q then of_z numerator else { q with numerator }

  let add ?(poll = no_poll) a b =
    poll ();
    if is_zero a
    then b
    else if is_zero b
    then a
    else if is_integer a && is_integer b
    then of_z (Z.add ~poll a.numerator b.numerator)
    else if is_integer a
    then
      { numerator =
          Z.add ~poll (Z.mul ~poll a.numerator b.denominator) b.numerator;
        denominator = b.denominator
      }
    else if is_integer b
    then
      { numerator =
          Z.add ~poll a.numerator (Z.mul ~poll b.numerator a.denominator);
        denominator = a.denominator
      }
    else if Z.equal a.denominator b.denominator
    then make ~poll (Z.add ~poll a.numerator b.numerator) a.denominator
    else
      let common = Z.gcd ~poll a.denominator b.denominator in
      let ad = exact_div ~poll a.denominator common in
      let bd = exact_div ~poll b.denominator common in
      let numerator =
        Z.add ~poll (Z.mul ~poll a.numerator bd) (Z.mul ~poll b.numerator ad)
      in
      let divisor = Z.gcd ~poll numerator common in
      { numerator = exact_div ~poll numerator divisor;
        denominator =
          Z.mul ~poll ad (exact_div ~poll b.denominator divisor)
      }

  let sub ?(poll = no_poll) a b = add ~poll a (neg b)

  let mul ?(poll = no_poll) a b =
    poll ();
    if is_zero a || is_zero b
    then zero
    else if is_integer a && is_integer b
    then of_z (Z.mul ~poll a.numerator b.numerator)
    else if is_integer a || is_integer b
    then
      let integer, fraction = if is_integer a then a, b else b, a in
      if Z.equal integer.numerator Z.one
      then fraction
      else if Z.equal integer.numerator Z.minus_one
      then neg fraction
      else
        let common = Z.gcd ~poll integer.numerator fraction.denominator in
        { numerator =
            Z.mul ~poll (exact_div ~poll integer.numerator common)
              fraction.numerator;
          denominator = exact_div ~poll fraction.denominator common
        }
    else
      let ac = Z.gcd ~poll a.numerator b.denominator in
      let bc = Z.gcd ~poll b.numerator a.denominator in
      { numerator =
          Z.mul ~poll (exact_div ~poll a.numerator ac)
            (exact_div ~poll b.numerator bc);
        denominator =
          Z.mul ~poll (exact_div ~poll a.denominator bc)
            (exact_div ~poll b.denominator ac)
      }

  let div ?(poll = no_poll) a b =
    poll ();
    if is_zero b then raise Division_by_zero;
    if is_zero a then zero
    else if is_integer b && Z.equal b.numerator Z.one then a
    else if is_integer b && Z.equal b.numerator Z.minus_one then neg a
    else if is_integer a && is_integer b
    then make ~poll a.numerator b.numerator
    else if equal a b then one
    else
      let inverse =
        if sign b < 0
        then
          { numerator = Z.neg b.denominator;
            denominator = Z.neg b.numerator
          }
        else { numerator = b.denominator; denominator = b.numerator }
      in
      mul ~poll a inverse

  let floor ?(poll = no_poll) q =
    poll ();
    if is_integer q then q.numerator
    else Z.floor_div ~poll q.numerator q.denominator

  let ceil ?(poll = no_poll) q =
    poll ();
    if is_integer q then q.numerator
    else Z.ceil_div ~poll q.numerator q.denominator

  let to_string q =
    if is_integer q
    then Z.to_string q.numerator
    else Z.to_string q.numerator ^ "/" ^ Z.to_string q.denominator
end
