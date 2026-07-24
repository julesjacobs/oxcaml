(* This is the executable counterpart of the mathematical-integer sort used by
   refinement checking.  Its representation is deliberately hidden: values
   are canonical sign-magnitude integers with immutable, least-significant-first
   half-word limbs. *)

let radix_bits = (Sys.int_size - 1) / 2
let radix = 1 lsl radix_bits
let mask = radix - 1

let decimal_chunk, decimal_chunk_width =
  if Sys.int_size <= 31 then 10_000, 4 else 1_000_000_000, 9
;;

let () =
  let largest_remainder = decimal_chunk - 1 in
  if largest_remainder > (max_int - mask) / radix
  then invalid_arg "Bigint: unsafe decimal chunk"
;;

type t : immutable_data =
  { sign : int
  ; mag : int list
  }

external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"
external int_greater : int -> int -> bool @@ total = "%greaterthan"

let (int_compare @ total) left right =
  if int_equal left right then 0 else if int_less left right then -1 else 1
;;

let (make @ total) sign mag =
  match mag with
  | [] -> { sign = 0; mag = [] }
  | _ -> { sign = (if int_less sign 0 then -1 else 1); mag }
;;

let zero @ logical = { sign = 0; mag = [] }
let (is_zero @ total) value = int_equal value.sign 0

let rec compare_magnitude @ total = fun left right ->
  match left with
  | [] ->
    (match right with
     | [] -> 0
     | _ :: _ -> -1)
  | left_limb :: left_rest ->
    (match right with
     | [] -> 1
     | right_limb :: right_rest ->
       let higher_order = compare_magnitude left_rest right_rest in
       if int_equal higher_order 0
       then int_compare left_limb right_limb
       else higher_order)
;;

let rec add_carry @ total = fun carry magnitude ->
  match magnitude with
  | [] -> if int_equal carry 0 then [] else [ carry ]
  | limb :: rest ->
    let sum = limb + carry in
    (sum land mask) :: add_carry (sum lsr radix_bits) rest
;;

let rec add_magnitude @ total = fun carry left right ->
  match left with
  | [] -> add_carry carry right
  | left_limb :: left_rest ->
    (match right with
     | [] ->
       let sum = left_limb + carry in
       (sum land mask)
       :: add_magnitude (sum lsr radix_bits) left_rest []
     | right_limb :: right_rest ->
       let sum = left_limb + right_limb + carry in
       (sum land mask)
       :: add_magnitude
            (sum lsr radix_bits)
            left_rest
            right_rest)
;;

(* [left] must be at least [right].  Omitting a final zero while recursion
   unwinds preserves the canonical no-high-zero representation. *)
let rec subtract_magnitude @ total = fun borrow left right ->
  match left with
  | [] -> []
  | left_limb :: left_rest ->
    let difference, right_rest =
      match right with
      | [] -> left_limb - borrow, []
      | right_limb :: right_rest ->
        left_limb - right_limb - borrow, right_rest
    in
    let digit, next_borrow =
      if int_less difference 0
      then difference + radix, 1
      else difference, 0
    in
    let rest = subtract_magnitude next_borrow left_rest right_rest in
    (match rest with
     | [] when int_equal digit 0 -> []
     | _ -> digit :: rest)
;;

let rec multiply_digit @ total = fun digit magnitude carry ->
  match magnitude with
  | [] -> if int_equal carry 0 then [] else [ carry ]
  | limb :: rest ->
    let product = (digit * limb) + carry in
    (product land mask)
    :: multiply_digit digit rest (product lsr radix_bits)
;;

(* Schoolbook multiplication, expressed as structural recursion over the left
   magnitude.  Half-word limbs keep every digit product within [max_int]. *)
let rec multiply_magnitude @ total = fun left right ->
  match left with
  | [] -> []
  | left_limb :: left_rest ->
    let upper = multiply_magnitude left_rest right in
    let shifted_upper =
      match upper with
      | [] -> []
      | _ -> 0 :: upper
    in
    add_magnitude 0 (multiply_digit left_limb right 0) shifted_upper
;;

let (compare @ total) left right =
  if not (int_equal left.sign right.sign)
  then int_compare left.sign right.sign
  else
    match left.sign with
    | 0 -> 0
    | sign ->
      let magnitude_order = compare_magnitude left.mag right.mag in
      if int_greater sign 0 then magnitude_order else -magnitude_order
;;

let (equal @ total) left right = int_equal (compare left right) 0
let (lt @ total) left right = int_less (compare left right) 0
let (le @ total) left right = not (int_greater (compare left right) 0)
let (gt @ total) left right = int_greater (compare left right) 0
let (ge @ total) left right = not (int_less (compare left right) 0)

let (neg @ total) value =
  if int_equal value.sign 0
  then zero
  else { sign = -value.sign; mag = value.mag }
;;

let (abs @ total) value =
  if int_equal value.sign 0 then zero else { sign = 1; mag = value.mag }
;;

let (add @ total) left right =
  match left.sign, right.sign with
  | 0, _ -> right
  | _, 0 -> left
  | left_sign, right_sign when int_equal left_sign right_sign ->
    make left_sign (add_magnitude 0 left.mag right.mag)
  | left_sign, _ ->
    let magnitude_order = compare_magnitude left.mag right.mag in
    if int_equal magnitude_order 0
    then zero
    else if int_greater magnitude_order 0
    then make left_sign (subtract_magnitude 0 left.mag right.mag)
    else make (-left_sign) (subtract_magnitude 0 right.mag left.mag)
;;

let (sub @ total) left right = add left (neg right)

let (mul @ total) left right =
  if int_equal left.sign 0 || int_equal right.sign 0
  then zero
  else
    make
      (left.sign * right.sign)
      (multiply_magnitude left.mag right.mag)
;;

let (of_int @ total) integer =
  if int_equal integer 0
  then zero
  else if int_equal integer min_int
  then
    let magnitude_bits = Sys.int_size - 1 in
    let top =
      1 lsl (magnitude_bits - (2 * radix_bits))
    in
    { sign = -1; mag = [ 0; 0; top ] }
  else
    let sign, magnitude =
      if int_less integer 0 then -1, -integer else 1, integer
    in
    let low = magnitude land mask in
    let high = magnitude lsr radix_bits in
    let mag = if int_equal high 0 then [ low ] else [ low; high ] in
    { sign; mag }
;;

let one @ logical = of_int 1

let to_int_opt value =
  match value.sign, value.mag with
  | 0, [] -> Some 0
  | sign, [ low ] -> Some (sign * low)
  | sign, [ low; high ] -> Some (sign * (low + (high * radix)))
  | -1, [ 0; 0; top ] ->
    let magnitude_bits = Sys.int_size - 1 in
    let expected_top = 1 lsl (magnitude_bits - (2 * radix_bits)) in
    if top = expected_top then Some min_int else None
  | _, _ -> None
;;

let divide_magnitude_small magnitude divisor =
  let rec divide remainder started quotient = function
    | [] -> quotient, remainder
    | limb :: rest ->
      let current = (remainder * radix) + limb in
      let digit = current / divisor in
      let remainder = current mod divisor in
      if digit = 0 && not started
      then divide remainder false quotient rest
      else divide remainder true (digit :: quotient) rest
  in
  divide 0 false [] (List.rev magnitude)
;;

let to_string value =
  if value.sign = 0
  then "0"
  else begin
    let rec collect magnitude chunks =
      match magnitude with
      | [] -> chunks
      | _ ->
        let quotient, remainder =
          divide_magnitude_small magnitude decimal_chunk
        in
        collect quotient (remainder :: chunks)
    in
    match collect value.mag [] with
    | [] -> "0"
    | most_significant :: rest ->
      let buffer = Buffer.create 32 in
      if value.sign < 0 then Buffer.add_char buffer '-';
      Buffer.add_string buffer (string_of_int most_significant);
      List.iter
        (fun digits ->
          Buffer.add_string buffer
            (Printf.sprintf "%0*d" decimal_chunk_width digits))
        rest;
      Buffer.contents buffer
  end
;;

let of_string string =
  let length = String.length string in
  if length = 0 then invalid_arg "Bigint.of_string: empty";
  let negative, first = if string.[0] = '-' then true, 1 else false, 0 in
  if first >= length then invalid_arg "Bigint.of_string: no digits";
  let digits = String.sub string first (length - first) in
  String.iter
    (fun character ->
      if character < '0' || character > '9'
      then invalid_arg "Bigint.of_string: non-digit")
    digits;
  if String.length digits > 1 && digits.[0] = '0'
  then invalid_arg "Bigint.of_string: leading zero";
  if negative && digits = "0"
  then invalid_arg "Bigint.of_string: negative zero";
  let ten = of_int 10 in
  let result = ref zero in
  String.iter
    (fun character ->
      result :=
        add
          (mul !result ten)
          (of_int (Char.code character - Char.code '0')))
    digits;
  if negative then neg !result else !result
;;
