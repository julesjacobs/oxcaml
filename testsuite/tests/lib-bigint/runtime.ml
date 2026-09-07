(* TEST *)

module B = Bigint

let (total_add @ total) x y = B.add x y
let (total_zero @ total) = B.zero

let of_decimal s =
  let negative = s.[0] = '-' in
  let n = ref B.zero in
  for i = (if negative then 1 else 0) to String.length s - 1 do
    n := B.add (B.mul !n (B.of_int 10))
        (B.of_int (Char.code s.[i] - Char.code '0'))
  done;
  if negative then B.neg !n else !n

let check_decimal expected actual =
  let actual = B.to_string actual in
  if actual <> expected then
    failwith (Printf.sprintf "expected %s, got %s" expected actual)

let abs n = if B.compare n B.zero < 0 then B.neg n else n

let check_division a b =
  let q = B.div a b and r = B.modulo a b in
  assert (a = B.add (B.mul b q) r);
  if b = B.zero then assert (q = B.zero && r = a)
  else assert (B.compare r B.zero >= 0 && B.compare r (abs b) < 0)

let check_pair a b =
  assert (B.sub (B.add a b) b = a);
  assert (B.neg (B.sub a b) = B.sub b a);
  assert (B.mul a b = B.mul b a);
  assert (B.mul (B.add a B.one) b = B.add (B.mul a b) b);
  let cmp = Stdlib.compare a b in
  assert (cmp = B.compare a b);
  assert (B.equal a b = (a = b));
  assert (B.(a < b) = (a < b));
  assert (B.(a <= b) = (a <= b));
  assert (B.(a > b) = (a > b));
  assert (B.(a >= b) = (a >= b));
  assert (B.(a <> b) = (a <> b));
  assert (B.(a + b) = B.add a b);
  assert (B.(a - b) = B.sub a b);
  assert (B.(a * b) = B.mul a b);
  assert (B.(a / b) = B.div a b);
  assert (B.(a mod b) = B.modulo a b);
  assert (B.(-a) = B.neg a);
  check_division a b;
  let copy = Marshal.from_string (Marshal.to_string a []) 0 in
  assert (copy = a);
  assert (Hashtbl.hash copy = Hashtbl.hash a);
  assert (Hashtbl.hash (B.sub (B.add a b) b) = Hashtbl.hash a)

let check_small a b =
  let ba = B.of_int a and bb = B.of_int b in
  assert (B.to_int_opt (B.add ba bb) = Some (a + b));
  assert (B.to_int_opt (B.sub ba bb) = Some (a - b));
  assert (B.to_int_opt (B.mul ba bb) = Some (a * b));
  assert (B.compare ba bb = Int.compare a b);
  let q, r =
    if b = 0 then 0, a
    else
      let q = a / b and r = a mod b in
      if r >= 0 then q, r
      else (if b > 0 then q - 1 else q + 1), r + Stdlib.abs b
  in
  assert (B.to_int_opt (B.div ba bb) = Some q);
  assert (B.to_int_opt (B.modulo ba bb) = Some r)

let check_marshalling () =
  let data = Marshal.to_bytes B.one [] in
  let marker = "vox.bigint.v1" in
  let rec find i =
    if Bytes.sub_string data i (String.length marker) = marker then i
    else find (i + 1)
  in
  let count = find 0 + String.length marker + 1 + 12 in
  let reject edit =
    let data = Bytes.copy data in
    edit data;
    match (Marshal.from_bytes data 0 : B.t) with
    | _ -> failwith "accepted malformed bigint"
    | exception Failure _ -> ()
  in
  reject (fun data -> Bytes.set_int64_be data count Int64.min_int);
  reject (fun data -> Bytes.set_int64_be data count Int64.max_int);
  reject (fun data -> Bytes.set_int32_be data (count + 8) 0l);
  reject (fun data -> Bytes.set_int32_be data (count + 8) 1_000_000_000l);
  if Sys.word_size = 64 then
    reject (fun data -> Bytes.set_int64_be data (count - 8) 0L);
  let zero = B.sub B.one B.one in
  assert (Marshal.from_string (Marshal.to_string zero []) 0 = B.zero)

let () =
  Gc.set { (Gc.get ()) with minor_heap_size = 4096 };
  assert (total_add total_zero B.one = B.one);
  List.iter (fun n ->
    assert (B.to_int_opt (B.of_int n) = Some n);
    check_decimal (string_of_int n) (B.of_int n))
    [min_int; min_int + 1; -1; 0; 1; max_int - 1; max_int];
  assert (B.to_int_opt (B.sub (B.of_int min_int) B.one) = None);
  assert (B.to_int_opt (B.add (B.of_int max_int) B.one) = None);
  assert (B.to_int_opt (B.neg (B.of_int min_int)) = None);
  check_decimal "0" (B.neg B.zero);
  check_decimal "0" (B.mul B.zero (B.of_int max_int));
  check_decimal "0" (B.mul (B.of_int max_int) B.zero);
  let huge = of_decimal "123456789012345678901234567890" in
  check_decimal "123456789012345678901234567890" huge;
  check_decimal "-123456789012345678901234567890" (B.neg huge);
  assert (Format.asprintf "%a" B.pp huge = B.to_string huge);
  check_decimal "999999999999999998000000000000000001"
    (let n = of_decimal "999999999999999999" in B.mul n n);
  check_decimal "1000000000000000000000000000"
    (B.add (of_decimal "999999999999999999999999999") B.one);
  check_decimal "999999999999999999999999999"
    (B.sub (of_decimal "1000000000000000000000000000") B.one);
  let values = List.map of_decimal
      ["0"; "1"; "-1"; "999999999"; "1000000000"; "-1000000001";
       "1000000000000000000"; "-999999999999999999999999999";
       "123456789012345678901234567890";
       "-123456789012345678901234567890"] in
  List.iter (fun a -> List.iter (check_pair a) values) values;
  for a = -75 to 75 do
    for b = -75 to 75 do check_small a b done
  done;
  let rng = Random.State.make [|12345|] in
  let random_bigint () =
    let value = ref B.zero in
    for _ = 0 to Random.State.int rng 8 do
      value := B.add (B.mul !value (B.of_int 1_000_000_000))
          (B.of_int (Random.State.int rng 1_000_000_000))
    done;
    if Random.State.bool rng then B.neg !value else !value
  in
  for i = 0 to 999 do
    check_pair (random_bigint ()) (random_bigint ());
    if i mod 100 = 0 then Gc.compact ()
  done;
  let distinct_a = Sys.opaque_identity (B.of_int 12345) in
  let distinct_b = Sys.opaque_identity (B.of_int 12345) in
  assert (distinct_a = distinct_b && distinct_a != distinct_b);
  check_marshalling ();
  print_endline "Bigint runtime: ok"
