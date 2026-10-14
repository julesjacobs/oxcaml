module Z = Vox_smt_integer.Z
module Q = Vox_smt_integer.Q

let check condition message = if not condition then failwith message

let expect text value =
  let actual = Z.to_string value in
  if actual <> text
  then failwith (Printf.sprintf "Expected %s, got %s" text actual)

let small () =
  for a = -80 to 80 do
    for b = -80 to 80 do
      let x = Z.of_int a and y = Z.of_int b in
      expect (string_of_int (a + b)) (Z.add x y);
      expect (string_of_int (a - b)) (Z.sub x y);
      expect (string_of_int (a * b)) (Z.mul x y);
      if b <> 0
      then begin
        let q, r = Z.quot_rem x y in
        expect (string_of_int (a / b)) q;
        expect (string_of_int (a mod b)) r;
        let q, r = Z.div_rem x y in
        let remainder = ((a mod b) + abs b) mod abs b in
        expect (string_of_int remainder) r;
        expect (string_of_int ((a - remainder) / b)) q;
        let floor = int_of_float (floor (float a /. float b)) in
        let ceil = int_of_float (ceil (float a /. float b)) in
        expect (string_of_int floor) (Z.floor_div x y);
        expect (string_of_int ceil) (Z.ceil_div x y)
      end
    done
  done

let boundaries () =
  List.iter
    (fun n ->
      let z = Z.of_int n in
      expect (string_of_int n) z;
      check (Z.to_int_opt (Z.of_string (Z.to_string z)) = Some n)
        "int roundtrip";
      check (Z.equal (Z.neg (Z.neg z)) z) "double negation";
      check (Z.equal (Z.sub (Z.add z Z.one) Z.one) z) "boundary addition";
      check (Z.equal (Z.add (Z.sub z Z.one) Z.one) z) "boundary subtraction";
      List.iter
        (fun divisor ->
          let q, r = Z.quot_rem z (Z.of_int divisor) in
          check
            (Z.equal (Z.add (Z.mul q (Z.of_int divisor)) r) z)
            "boundary division")
        [min_int; max_int; -1; 1; -31; 31])
    [min_int; min_int + 1; max_int; max_int - 1; 0; -1; 1];
  check (Z.to_int_opt (Z.neg (Z.of_int min_int)) = None) "positive min_int";
  check (Z.to_int_opt (Z.add (Z.of_int max_int) Z.one) = None) "overflow";
  List.iter
    (fun text ->
      match Z.of_string text with
      | _ -> failwith "invalid numeral accepted"
      | exception Invalid_argument _ -> ())
    [""; "+"; "-"; "1x"; " 1"; "1 "; "--1"];
  List.iter
    (fun action ->
      match action () with
      | _ -> failwith "zero divisor accepted"
      | exception Division_by_zero -> ())
    [(fun () -> ignore (Z.div_rem Z.one Z.zero));
     (fun () -> ignore (Q.make Z.one Z.zero));
     (fun () -> ignore (Q.div Q.one Q.zero))]

let canonical_boundaries () =
  let minimum = Z.of_int min_int and maximum = Z.of_int max_int in
  let values =
    [|Z.sub minimum Z.one, None;
      minimum, Some min_int;
      Z.add minimum Z.one, Some (min_int + 1);
      Z.minus_one, Some (-1);
      Z.zero, Some 0;
      Z.one, Some 1;
      Z.sub maximum Z.one, Some (max_int - 1);
      maximum, Some max_int;
      Z.neg minimum, None;
      Z.add (Z.neg minimum) Z.one, None|]
  in
  Array.iteri (fun i (value, small) ->
    check (Z.to_int_opt value = small) "canonical boundary representation";
    let negated = Z.neg value in
    check (Z.to_int_opt (Z.neg negated) = small)
      "canonical double negation";
    let parsed = Z.of_string (Z.to_string value) in
    check (Z.to_int_opt parsed = small) "parsed boundary representation";
    Array.iteri (fun j (other, _) ->
      check (Int.compare (Z.compare value other) 0 = Int.compare i j)
        "boundary ordering";
      check (Int.compare (Z.compare negated (Z.neg other)) 0
             = Int.compare j i) "negated boundary ordering";
      check (Z.equal value other = (i = j)) "boundary equality") values)
    values;
  List.iter (fun value ->
    check (Z.to_int_opt value = Some min_int) "canonical negative minimum")
    [Z.neg (Z.add maximum Z.one);
     Z.mul (Z.add maximum Z.one) Z.minus_one;
     fst (Z.quot_rem (Z.add maximum Z.one) Z.minus_one)]

let rational () =
  for a = -10 to 10 do
    for b = 1 to 12 do
      let x = Q.make (Z.of_int a) (Z.of_int b) in
      expect (string_of_int (int_of_float (floor (float a /. float b))))
        (Q.floor x);
      expect (string_of_int (int_of_float (ceil (float a /. float b))))
        (Q.ceil x);
      for c = -10 to 10 do
        for d = 1 to 12 do
          let x = Q.make (Z.of_int a) (Z.of_int b) in
          let y = Q.make (Z.of_int c) (Z.of_int d) in
          let expected n e = Q.make (Z.of_int n) (Z.of_int e) in
          check (Q.equal (Q.add x y) (expected ((a * d) + (b * c)) (b * d)))
            "rational addition";
          check (Q.equal (Q.sub x y) (expected ((a * d) - (b * c)) (b * d)))
            "rational subtraction";
          check (Q.equal (Q.mul x y) (expected (a * c) (b * d)))
            "rational multiplication";
          if c <> 0
          then check (Q.equal (Q.div x y) (expected (a * d) (b * c)))
              "rational division";
          check
            (Int.compare (Q.compare x y) 0 = Int.compare (a * d) (b * c))
            "rational comparison"
        done
      done
    done
  done

let rational_boundaries () =
  List.iter
    (fun value ->
      let integer = Q.of_z value in
      let third = Q.make value (Z.of_int 3) in
      check (Z.equal (Q.floor integer) value) "integer floor";
      check (Z.equal (Q.ceil integer) value) "integer ceiling";
      check (Q.equal (Q.div third Q.one) third) "division by one";
      check (Q.equal (Q.div third (Q.of_int (-1))) (Q.neg third))
        "division by minus one";
      check (Q.equal (Q.mul third Q.one) third) "multiply by one";
      check (Q.equal (Q.mul third (Q.of_int (-1))) (Q.neg third))
        "multiply by minus one";
      check (Q.equal (Q.mul integer third) (Q.div (Q.mul integer integer)
                                            (Q.of_int 3)))
        "integer rational multiplication";
      check (Q.equal (Q.add integer third) (Q.mul (Q.of_int 4) third))
        "integer rational addition")
    [Z.of_int min_int; Z.of_int max_int; Z.pow (Z.of_int 2) 2048;
     Z.neg (Z.pow (Z.of_int 2) 2048)]

exception Cancelled

let interruption () =
  let x = Z.sub (Z.pow (Z.of_int 2) 1024) Z.one in
  let before = Z.to_string x in
  let remaining = ref 5 in
  let poll () = decr remaining; if !remaining = 0 then raise Cancelled in
  (match Z.mul ~poll x x with
  | _ -> failwith "missed cancellation"
  | exception Cancelled -> ());
  check (Z.to_string x = before) "mutated cancelled operand";
  let square = Z.mul x x in
  let quotient, remainder = Z.quot_rem square x in
  check (Z.equal quotient x && Z.is_zero remainder)
    "division after cancellation"

let large_vectors () =
  let state = ref 27183 in
  let random bound =
    state := ((!state * 25173) + 13849) land 65535;
    !state mod bound
  in
  let number maximum =
    let length = 1 + random maximum in
    let negative = random 2 = 0 in
    let digits = Bytes.create length in
    Bytes.set digits 0 (Char.chr (Char.code '1' + random 9));
    for i = 1 to length - 1 do
      Bytes.set digits i (Char.chr (Char.code '0' + random 10))
    done;
    Z.of_string ((if negative then "-" else "") ^ Bytes.to_string digits)
  in
  let output = Buffer.create 100_000 in
  let emit value =
    Buffer.add_string output (Z.to_string value);
    Buffer.add_char output '\n'
  in
  for _ = 1 to 512 do
    let a = number 420 in
    let b = number 280 in
    emit (Z.add a b);
    emit (Z.mul a b);
    let q, r = Z.quot_rem a b in
    emit q;
    emit r;
    emit (Z.gcd a b)
  done;
  (* The digest was computed with Python's arbitrary-precision integers. *)
  check
    (Digest.to_hex (Digest.string (Buffer.contents output))
     = "947fcfe7971a832063aa4ad6b97faf10")
    "large integer differential digest"

let integer_extensions () =
  let require = function Some value -> value
    | None -> failwith "Unexpected materialization limit" in
  for k = -3 to 10 do
    let mask = if k <= 0 then 0 else (1 lsl k) - 1 in
    for left = -25 to 25 do
      for right = -25 to 25 do
        expect (string_of_int (left land right land mask))
          (require (Z.piand (Z.of_int k) (Z.of_int left) (Z.of_int right)))
      done
    done
  done;
  List.iter (fun exponent ->
    let power = require (Z.pow2 (Z.of_int exponent)) in
    check (Z.equal power (Z.pow (Z.of_int 2) exponent)) "Power boundary";
    check (Z.equal (Z.log2 power) (Z.of_int exponent)) "Log power boundary";
    if exponent > 0 then
      check (Z.equal (Z.log2 (Z.sub power Z.one)) (Z.of_int (exponent - 1)))
        "Log predecessor boundary") [0; 1; 14; 15; 29; 30; 59; 60; 61; 62;
                                     63; 64; 127; 1024; 4096];
  List.iter (fun value -> expect "0" (Z.log2 (Z.of_int value)))
    [min_int; -1024; -1; 0; 1];
  check (Z.pow2 (Z.of_int 1_048_576) = None) "Power materialization limit";
  let huge = Z.of_string "999999999999999999999999999999999" in
  check (Z.pow2 huge = None) "Huge power materialization limit";
  check (Z.piand huge Z.minus_one Z.minus_one = None)
    "Bitwise materialization limit";
  expect "31" (require (Z.piand huge Z.minus_one (Z.of_int 31)));
  expect "0" (require (Z.pow2 (Z.neg huge)));
  expect "0" (require (Z.piand (Z.neg huge) Z.minus_one Z.minus_one));
  List.iter (fun (width, left, right, expected) ->
    expect expected (require (Z.piand (Z.of_int width)
      (Z.of_string left) (Z.of_string right))))
    [
      14, "-89680",
        "11390",
        "8240";
      15, "81457",
        "159817",
        "12289";
      29, "-197484674",
        "3348795344",
        "68849488";
      30, "-5480472230",
        "-2922515404",
        "289710096";
      59, "-4035914696991886205",
        "872183134584852832",
        "295140738482835456";
      60, "-2854010331381388252",
        "-8320804678422822833",
        "577727634515099652";
      127, "-858186733685588833146193031565186817953",
        "850206491171090266823968494927003330571",
        "162327106594443635638941257962274955275";
      500, "-31238198675430644320962161182087901406998210779593115045541\
         419073222751948463649260409188199865856023715726276094721463\
         71529394944357228139513844186505",
        "136461028260922010780248627065020762148423501782993444158047\
         772835429566354019311430148859730453918588187030279768745112\
         60234264469839958395819465228264",
        "143151155580905613164769924946106839834537446642813812297975\
         629586711871369814422843432762292484342719651322063163445638\
         202893343155070981881932546656"];
  List.iter (fun operation ->
    let polls = ref 0 in
    (match operation (fun () -> incr polls; if !polls = 3 then raise Exit) with
    | exception Exit -> ()
    | _ -> failwith "Integer extension did not poll");
    check (!polls = 3) "Integer extension poll count")
    [(fun poll -> ignore (Z.pow2 ~poll (Z.of_int 4096)));
     (fun poll -> ignore (Z.piand ~poll (Z.of_int 4096)
       Z.minus_one Z.minus_one))]

let () =
  integer_extensions ();
  large_vectors ();
  small ();
  boundaries ();
  canonical_boundaries ();
  rational ();
  rational_boundaries ();
  interruption ();
  print_endline "Exact integer and rational tests passed"
