type bytes : immutable_data mod total = int list
type request : immutable_data mod total = {
  request_line : bytes;
  headers : int list list;
  body : bytes;
}
type malformed : immutable_data mod total =
  | Invalid_byte | Invalid_crlf | Invalid_request_line | Invalid_header
  | Invalid_content_length | Conflicting_content_length
  | Unsupported_transfer_encoding | Transfer_encoding_content_length
  | Invalid_host
[@@inductive]
type resource : immutable_data mod total = Message_bytes | Body_bytes
[@@inductive]
type framing : immutable_data mod total =
  | Length of int | Bad of malformed | Too_large of resource
[@@inductive]
let[@def] rec equal_bytes (left : bytes) (right : bytes) =
  match left, right with
  | [], [] -> true
  | x :: xs, y :: ys -> x = y && equal_bytes xs ys
  | _ -> false
let[@def] byte b = 0 <= b && b <= 255
let[@def] token b =
  (65 <= b && b <= 90) || (97 <= b && b <= 122)
  || (48 <= b && b <= 57) || b = 33 || b = 35 || b = 36 || b = 37
  || b = 38 || b = 39 || b = 42 || b = 43 || b = 45 || b = 46
  || b = 94 || b = 95 || b = 96 || b = 124 || b = 126
let[@def] visible b = 33 <= b && b <= 126
let[@def] value_byte b = b = 9 || (32 <= b && b <= 126) || (128 <= b && b <=
  255)
let[@def] lower b = if 65 <= b && b <= 90 then b + 32 else b
let[@def] rec lower_all xs = match xs with [] -> [] | b :: bs -> lower b ::
  lower_all bs
let[@def] rec all_token xs = match xs with [] -> true | b :: bs -> token b &&
  all_token bs
let[@def] rec all_visible xs = match xs with [] -> true | b :: bs -> visible b
  && all_visible bs
let[@def] rec all_value xs = match xs with [] -> true | b :: bs -> value_byte b
  && all_value bs
let[@def] nonempty (xs : bytes) = match xs with [] -> false | _ :: _ -> true
let[@def] rec split (delimiter : int) (xs : bytes) =
  match xs with
  | [] -> ([], [])
  | b :: bs -> if b = delimiter then ([], bs)
    else let (before, after) = split delimiter bs in (b :: before, after)
let[@def] rec trim_left xs =
  match xs with b :: bs when b = 32 || b = 9 -> trim_left bs | _ -> xs
let[@def] rec trim_right xs =
  match xs with
  | [] -> []
  | b :: bs -> let rest = trim_right bs in
    if (b = 32 || b = 9) && not (nonempty rest) then [] else b :: rest
let[@def] trim xs = trim_right (trim_left xs)
let[@def] valid_request_line line =
  let (meth, rest) = split 32 line in
  let (target, version) = split 32 rest in
  nonempty meth && all_token meth && nonempty target && all_visible target
  && equal_bytes version [72; 84; 84; 80; 47; 49; 46; 49]
let[@def] header line =
  let (name, value) = split 58 line in
  if nonempty name && all_token name && all_value value
  then Some (lower_all name, trim value) else None
let[@def] rec has_colon line =
  match line with [] -> false | b :: bs -> b = 58 || has_colon bs
let[@def] valid_header line = has_colon line && (match header line with None ->
  false | Some _ -> true)
let[@def] request_parts request =
  let (meth, rest) = split 32 request.request_line in
  let (target, _) = split 32 rest in
  (meth, target)
let[@def] header_field line =
  if valid_header line then header line else None
let[@def] rec decimal value digits =
  match digits with
  | [] -> Length value
  | b :: bs ->
    if b < 48 || b > 57 then Bad Invalid_content_length
    else if value > 819 || (value = 819 && b > 50) then Too_large Body_bytes
    else decimal (value * 10 + b - 48) bs
let[@def] content_length digits =
  if nonempty digits then decimal 0 digits else Bad Invalid_content_length
let[@def] is_cl name = equal_bytes name
  [99;111;110;116;101;110;116;45;108;101;110;103;116;104]
let[@def] is_host name = equal_bytes name [104;111;115;116]
let[@def] rec has_name (which : bytes) (headers : int list list) =
  match headers with
  | [] -> false
  | line :: rest -> let (name, _) = split 58 line in
    equal_bytes (lower_all name) which || has_name which rest
let[@def] rec frame_fields headers previous hosts =
  match headers with
  | [] -> if hosts <> 1 then Bad Invalid_host
    else (match previous with None -> Length 0 | Some n -> Length n)
  | line :: rest ->
    if not (valid_header line) then Bad Invalid_header
    else match header line with
    | None -> Bad Invalid_header
    | Some (name, value) ->
      if is_host name then
        if hosts <> 0 || not (nonempty value) then Bad Invalid_host
        else frame_fields rest previous 1
      else if is_cl name then
        (match content_length value with
        | Bad e -> Bad e | Too_large r -> Too_large r
        | Length n -> match previous with
          | Some old when old <> n -> Bad Conflicting_content_length
          | _ -> frame_fields rest (Some n) hosts)
      else frame_fields rest previous hosts
let[@def] framing headers =
  if has_name [116;114;97;110;115;102;101;114;45;101;110;99;111;100;105;110;103]
    headers then
    if has_name [99;111;110;116;101;110;116;45;108;101;110;103;116;104] headers
    then Bad Transfer_encoding_content_length else Bad
      Unsupported_transfer_encoding
  else frame_fields headers None 0
let[@def] rec wire_headers headers tail =
  match headers with [] -> 13 :: 10 :: tail
  | line :: rest -> Vox_sequence.append line (13 :: 10 :: wire_headers rest
    tail)
let[@def] serialize request =
  Vox_sequence.append request.request_line (13 :: 10 :: wire_headers
    request.headers
    request.body)
let[@def] rec safe_line line =
  match line with [] -> true | b :: bs -> byte b && b <> 13 && b <> 10 &&
    safe_line bs
let[@def] rec header_lines headers =
  match headers with [] -> true
  | line :: rest -> nonempty line && safe_line line && valid_header line &&
    header_lines rest
let[@def] rec sized n bytes =
  match bytes with [] -> n = 0
  | b :: rest -> n > 0 && byte b && sized (n - 1) rest
let[@def] rec fits budget bytes =
  match bytes with [] -> budget >= 0
  | _ :: rest -> budget > 0 && fits (budget - 1) rest
let[@def] well_formed request =
  valid_request_line request.request_line && safe_line request.request_line
  && header_lines request.headers
  && (match framing request.headers with Length n -> sized n request.body | _ ->
    false)
  && fits 16384 (serialize request)

let[@def] has_transfer_encoding headers =
  has_name
    [116;114;97;110;115;102;101;114;45;101;110;99;111;100;105;110;103] headers
let[@def] has_content_length headers =
  has_name [99;111;110;116;101;110;116;45;108;101;110;103;116;104] headers

let[@def] rec content_lengths_match headers n = ghost_ (
  match headers with
  | [] -> true
  | line :: rest ->
    (match header_field line with
     | None -> false
     | Some (name, value) ->
       (if is_cl name then content_length value === Length n else true)
       && content_lengths_match rest n))

type status : immutable_data mod total =
  | Incomplete
  | Complete of request
  | Malformed of malformed
  | Limit of resource
[@@inductive]

let[@def] is_terminal status =
  match status with Incomplete -> false | _ -> true
