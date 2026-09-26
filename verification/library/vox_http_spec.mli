type bytes : immutable_data mod total = int list

type request : immutable_data mod total = { request_line : bytes; headers :
  int list list; body : bytes; }

type malformed : immutable_data mod total = Invalid_byte | Invalid_crlf |
  Invalid_request_line | Invalid_header | Invalid_content_length |
  Conflicting_content_length | Unsupported_transfer_encoding |
  Transfer_encoding_content_length | Invalid_host [@@inductive]

type resource : immutable_data mod total = Message_bytes | Body_bytes
  [@@inductive]

type framing : immutable_data mod total = Length of int | Bad of malformed |
  Too_large of resource [@@inductive]

val equal_bytes : bytes -> bytes -> bool @@ total

val equal_bytes_def : (left : bytes) -> (right : bytes) -> {u : unit |
  (equal_bytes left right) === (match (left, right) with | ([], []) -> true |
  (x::xs, y::ys) -> (x = y) && (equal_bytes xs ys) | _ -> false)} @@ total

val byte : int -> bool @@ total

val byte_def : (b : int) -> {u : unit | (byte b) === ((0 <= b) && (b <= 255))}
  @@ total

val token : int -> bool @@ total

val token_def : (b : int) -> {u : unit | (token b) === (((65 <= b) && (b <=
  90)) || (((97 <= b) && (b <= 122)) || (((48 <= b) && (b <= 57)) || ((b = 33)
  || ((b = 35) || ((b = 36) || ((b = 37) || ((b = 38) || ((b = 39) || ((b =
  42) || ((b = 43) || ((b = 45) || ((b = 46) || ((b = 94) || ((b = 95) || ((b
  = 96) || ((b = 124) || (b = 126))))))))))))))))))} @@ total

val visible : int -> bool @@ total

val visible_def : (b : int) -> {u : unit | (visible b) === ((33 <= b) && (b <=
  126))} @@ total

val value_byte : int -> bool @@ total

val value_byte_def : (b : int) -> {u : unit | (value_byte b) === ((b = 9) ||
  (((32 <= b) && (b <= 126)) || ((128 <= b) && (b <= 255))))} @@ total

val lower : int -> int @@ total

val lower_def : (b : int) -> {u : unit | (lower b) === (if (65 <= b) && (b <=
  90) then b + 32 else b)} @@ total

val lower_all : int list -> int list @@ total

val lower_all_def : (xs : int list) -> {u : unit | (lower_all xs) === (match
  xs with | [] -> [] | b::bs -> (lower b) :: (lower_all bs))} @@ total

val all_token : int list -> bool @@ total

val all_token_def : (xs : int list) -> {u : unit | (all_token xs) === (match
  xs with | [] -> true | b::bs -> (token b) && (all_token bs))} @@ total

val all_visible : int list -> bool @@ total

val all_visible_def : (xs : int list) -> {u : unit | (all_visible xs) ===
  (match xs with | [] -> true | b::bs -> (visible b) && (all_visible bs))} @@
  total

val all_value : int list -> bool @@ total

val all_value_def : (xs : int list) -> {u : unit | (all_value xs) === (match
  xs with | [] -> true | b::bs -> (value_byte b) && (all_value bs))} @@ total

val nonempty : bytes -> bool @@ total

val nonempty_def : (xs : bytes) -> {u : unit | (nonempty xs) === (match xs
  with | [] -> false | _::_ -> true)} @@ total

val split : int -> bytes -> int list * int list @@ total

val split_def : (delimiter : int) -> (xs : bytes) -> {u : unit | (split
  delimiter xs) === (match xs with | [] -> ([], []) | b::bs -> if b =
  delimiter then ([], bs) else (match split delimiter bs with | (before,
  after) -> ((b :: before), after)))} @@ total

val trim_left : int list -> int list @@ total

val trim_left_def : (xs : int list) -> {u : unit | (trim_left xs) === (match
  xs with | b::bs when (b = 32) || (b = 9) -> trim_left bs | _ -> xs)} @@
  total

val trim_right : int list -> bytes @@ total

val trim_right_def : (xs : int list) -> {u : unit | (trim_right xs) === (match
  xs with | [] -> [] | b::bs -> let rest = trim_right bs in if ((b = 32) || (b
  = 9)) && (not (nonempty rest)) then [] else b :: rest)} @@ total

val trim : int list -> bytes @@ total

val trim_def : (xs : int list) -> {u : unit | (trim xs) === (trim_right
  (trim_left xs))} @@ total

val valid_request_line : bytes -> bool @@ total

val valid_request_line_def : (line : bytes) -> {u : unit | (valid_request_line
  line) === (match split 32 line with | (meth, rest) -> (match split 32 rest
  with | (target, version) -> (nonempty meth) && ((all_token meth) &&
  ((nonempty target) && ((all_visible target) && (equal_bytes version [72; 84;
  84; 80; 47; 49; 46; 49]))))))} @@ total

val header : bytes -> (int list * bytes) option @@ total

val header_def : (line : bytes) -> {u : unit | (header line) === (match split
  58 line with | (name, value) -> if (nonempty name) && ((all_token name) &&
  (all_value value)) then Some ((lower_all name), (trim value)) else None)} @@
  total

val has_colon : int list -> bool @@ total

val has_colon_def : (line : int list) -> {u : unit | (has_colon line) ===
  (match line with | [] -> false | b::bs -> (b = 58) || (has_colon bs))} @@
  total

val valid_header : bytes -> bool @@ total

val valid_header_def : (line : bytes) -> {u : unit | (valid_header line) ===
  ((has_colon line) && (match header line with | None -> false | Some _ ->
  true))} @@ total

val request_parts : request -> int list * int list @@ total

val request_parts_def : (request : request) -> {u : unit | (request_parts
  request) === (match split 32 request.request_line with | (meth, rest) ->
  (match split 32 rest with | (target, _) -> (meth, target)))} @@ total

val header_field : bytes -> (int list * bytes) option @@ total

val header_field_def : (line : bytes) -> {u : unit | (header_field line) ===
  (if valid_header line then header line else None)} @@ total

val decimal : int -> int list -> framing @@ total

val decimal_def : (value : int) -> (digits : int list) -> {u : unit | (decimal
  value digits) === (match digits with | [] -> Length value | b::bs -> if (b <
  48) || (b > 57) then Bad Invalid_content_length else if (value > 819) ||
  ((value = 819) && (b > 50)) then Too_large Body_bytes else decimal (((value
  * 10) + b) - 48) bs)} @@ total

val content_length : bytes -> framing @@ total

val content_length_def : (digits : bytes) -> {u : unit | (content_length
  digits) === (if nonempty digits then decimal 0 digits else Bad
  Invalid_content_length)} @@ total

val is_cl : bytes -> bool @@ total

val is_cl_def : (name : bytes) -> {u : unit | (is_cl name) === (equal_bytes
  name [99; 111; 110; 116; 101; 110; 116; 45; 108; 101; 110; 103; 116; 104])}
  @@ total

val is_host : bytes -> bool @@ total

val is_host_def : (name : bytes) -> {u : unit | (is_host name) ===
  (equal_bytes name [104; 111; 115; 116])} @@ total

val has_name : bytes -> int list list -> bool @@ total

val has_name_def : (which : bytes) -> (headers : int list list) -> {u : unit |
  (has_name which headers) === (match headers with | [] -> false | line::rest
  -> (match split 58 line with | (name, _) -> (equal_bytes (lower_all name)
  which) || (has_name which rest)))} @@ total

val frame_fields : bytes list -> int option -> int -> framing @@ total

val frame_fields_def : (headers : bytes list) -> (previous : int option) ->
  (hosts : int) -> {u : unit | (frame_fields headers previous hosts) ===
  (match headers with | [] -> if hosts <> 1 then Bad Invalid_host else (match
  previous with | None -> Length 0 | Some n' -> Length n') | line::rest -> if
  not (valid_header line) then Bad Invalid_header else (match header line with
  | None -> Bad Invalid_header | Some (name, value) -> if is_host name then
  (if (hosts <> 0) || (not (nonempty value)) then Bad Invalid_host else
  frame_fields rest previous 1) else if is_cl name then (match content_length
  value with | Bad e -> Bad e | Too_large r -> Too_large r | Length n ->
  (match previous with | Some old when old <> n -> Bad
  Conflicting_content_length | _ -> frame_fields rest (Some n) hosts)) else
  frame_fields rest previous hosts))} @@ total

val framing : bytes list -> framing @@ total

val framing_def : (headers : bytes list) -> {u : unit | (framing headers) ===
  (if has_name [116; 114; 97; 110; 115; 102; 101; 114; 45; 101; 110; 99; 111;
  100; 105; 110; 103] headers then (if has_name [99; 111; 110; 116; 101; 110;
  116; 45; 108; 101; 110; 103; 116; 104] headers then Bad
  Transfer_encoding_content_length else Bad Unsupported_transfer_encoding)
  else frame_fields headers None 0)} @@ total

val wire_headers : int Vox_sequence.t list -> int list -> int Vox_sequence.t
  @@ total

val wire_headers_def : (headers : int Vox_sequence.t list) -> (tail : int
  list) -> {u : unit | (wire_headers headers tail) === (match headers with |
  [] -> 13 :: 10 :: tail | line::rest -> Vox_sequence.append line (13 :: 10 ::
  (wire_headers rest tail)))} @@ total

val serialize : request -> int Vox_sequence.t @@ total

val serialize_def : (request : request) -> {u : unit | (serialize request) ===
  (Vox_sequence.append request.request_line (13 :: 10 :: (wire_headers
  request.headers request.body)))} @@ total

val safe_line : int list -> bool @@ total

val safe_line_def : (line : int list) -> {u : unit | (safe_line line) ===
  (match line with | [] -> true | b::bs -> (byte b) && ((b <> 13) && ((b <>
  10) && (safe_line bs))))} @@ total

val header_lines : bytes list -> bool @@ total

val header_lines_def : (headers : bytes list) -> {u : unit | (header_lines
  headers) === (match headers with | [] -> true | line::rest -> (nonempty
  line) && ((safe_line line) && ((valid_header line) && (header_lines
  rest))))} @@ total

val sized : int -> int list -> bool @@ total

val sized_def : (n : int) -> (bytes : int list) -> {u : unit | (sized n bytes)
  === (match bytes with | [] -> n = 0 | b::rest -> (n > 0) && ((byte b) &&
  (sized (n - 1) rest)))} @@ total

val fits : int -> bytes -> bool @@ total

val fits_def : (budget : int) -> (bytes : bytes) -> {u : unit | (fits budget
  bytes) === (match bytes with | [] -> budget >= 0 | _::rest -> (budget > 0)
  && (fits (budget - 1) rest))} @@ total

val well_formed : request -> bool @@ total

val well_formed_def : (request : request) -> {u : unit | (well_formed request)
  === ((valid_request_line request.request_line) && ((safe_line
  request.request_line) && ((header_lines request.headers) && ((match framing
  request.headers with | Length n -> sized n request.body | _ -> false) &&
  (fits 16384 (serialize request))))))} @@ total

val has_transfer_encoding : int list list -> bool @@ total

val has_transfer_encoding_def : (headers : int list list) -> {u : unit |
  (has_transfer_encoding headers) === (has_name [116; 114; 97; 110; 115; 102;
  101; 114; 45; 101; 110; 99; 111; 100; 105; 110; 103] headers)} @@ total

val has_content_length : int list list -> bool @@ total

val has_content_length_def : (headers : int list list) -> {u : unit |
  (has_content_length headers) === (has_name [99; 111; 110; 116; 101; 110;
  116; 45; 108; 101; 110; 103; 116; 104] headers)} @@ total

val content_lengths_match : bytes list @ total -> int @ total -> bool @ ghost
  @@ total

val content_lengths_match_def : (headers : bytes list) -> (n : int) -> {u :
  unit | (content_lengths_match headers n) === (ghost_ (match headers with |
  [] -> true | line::rest -> (match header_field line with | None -> false |
  Some (name, value) -> (if is_cl name then (content_length value) === (Length
  n) else true) && (content_lengths_match rest n))))} @@ total

type status : immutable_data mod total = Incomplete | Complete of request |
  Malformed of malformed | Limit of resource [@@inductive]

val is_terminal : status -> bool @@ total

val is_terminal_def : (status : status) -> {u : unit | (is_terminal status)
  === (match status with | Incomplete -> false | _ -> true)} @@ total
