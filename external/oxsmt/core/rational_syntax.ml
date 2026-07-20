type fraction_error =
  | Not_a_fraction
  | Invalid_signed_integer
  | Zero_denominator

let is_digits s =
  String.length s > 0
  && String.for_all
       (function
         | '0' .. '9' -> true
         | _ -> false)
       s
;;

let unsigned_integer s =
  if is_digits s
  then (
    match Bigint.of_string s with
    | n -> Some n
    | exception Invalid_argument _ -> None)
  else None
;;

let signed_integer ~atom ~minus value =
  match atom value with
  | Some digits -> unsigned_integer digits
  | None ->
    (match minus value with
     | Some magnitude -> Option.map Bigint.neg (Option.bind (atom magnitude) unsigned_integer)
     | None -> None)
;;

let fraction ~atom ~minus ~divide value =
  match divide value with
  | None -> Error Not_a_fraction
  | Some (num_s, den_s) ->
    (match signed_integer ~atom ~minus num_s, signed_integer ~atom ~minus den_s with
     | Some _, Some den when Bigint.is_zero den -> Error Zero_denominator
     | Some num, Some den -> Ok (num, den)
     | _ -> Error Invalid_signed_integer)
;;

(* SMT-LIB decimals have at least one digit on each side of the point.  Fold the
   numerator and the power-of-ten denominator directly as [Bigint] values: the number of
   fractional digits is never converted into a machine-sized exponent. *)
let decimal s =
  let ten = Bigint.of_string "10" in
  let num = ref Bigint.zero in
  let den = ref Bigint.one in
  let point_seen = ref false in
  let int_digit_seen = ref false in
  let frac_digit_seen = ref false in
  let valid = ref true in
  String.iter
    (fun c ->
       match c with
       | '0' .. '9' ->
         let digit = Bigint.of_string (String.make 1 c) in
         num := Bigint.add (Bigint.mul !num ten) digit;
         if !point_seen
         then (
           frac_digit_seen := true;
           den := Bigint.mul !den ten)
         else int_digit_seen := true
       | '.' when not !point_seen -> point_seen := true
       | _ -> valid := false)
    s;
  if !valid && !point_seen && !int_digit_seen && !frac_digit_seen
  then Some (!num, !den)
  else None
;;
