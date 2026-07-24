(* TEST
   bytecode;
   native;
*)

let checks = ref 0

let check condition =
  incr checks;
  if not condition then failwith "Bigint runtime check failed"
;;

let check_string string =
  check (String.equal (Bigint.to_string (Bigint.of_string string)) string)
;;

let boundaries =
  [ "0"
  ; "1"
  ; "-1"
  ; "2147483647"
  ; "2147483648"
  ; "2147483649"
  ; "4611686018427387903"
  ; "-4611686018427387904"
  ; "4611686018427387904"
  ; "9223372036854775807"
  ; "-9223372036854775808"
  ; "123456789012345678901234567890"
  ; "-999999999999999999999999999999999999"
  ]
;;

let invalid_strings = [ ""; "-"; "00"; "01"; "-0"; "+1"; " 1"; "1x" ]

let check_invalid string =
  match Bigint.of_string string with
  | _ -> check false
  | exception Invalid_argument _ -> check true
;;

(* Fixed-seed generator.  Restricting each step to 30 bits makes the result
   independent of machine-int overflow. *)
let random_state = ref 1_234_567

let random bound =
  random_state := (!random_state + 104_729) mod 1_000_003;
  !random_state mod bound
;;

let random_decimal max_digits =
  if random 20 = 0
  then "0"
  else
    let digits = 1 + random max_digits in
    let buffer = Buffer.create (digits + 1) in
    if random 2 = 0 then Buffer.add_char buffer '-';
    Buffer.add_char buffer (Char.chr (Char.code '1' + random 9));
    for _ = 2 to digits do
      Buffer.add_char buffer (Char.chr (Char.code '0' + random 10))
    done;
    Buffer.contents buffer
;;

let check_properties () =
  for _ = 1 to 1_000 do
    let left = Bigint.of_string (random_decimal 80) in
    let right = Bigint.of_string (random_decimal 80) in
    check
      (Bigint.equal
         (Bigint.of_string (Bigint.to_string left))
         left);
    check ((left = right) = Bigint.equal left right);
    check (Bigint.equal (Bigint.add left right) (Bigint.add right left));
    check (Bigint.equal (Bigint.sub (Bigint.add left right) right) left);
    check (Bigint.is_zero (Bigint.add left (Bigint.neg left)));
    check (Bigint.equal (Bigint.mul left right) (Bigint.mul right left));
    check (Bigint.equal (Bigint.abs left) (Bigint.abs (Bigint.neg left)));
    let order = Bigint.compare left right in
    check (Bigint.lt left right = (order < 0));
    check (Bigint.le left right = (order <= 0));
    check (Bigint.gt left right = (order > 0));
    check (Bigint.ge left right = (order >= 0))
  done
;;

let check_machine_boundaries () =
  List.iter
    (fun integer ->
      check (Bigint.to_int_opt (Bigint.of_int integer) = Some integer))
    [ min_int; min_int + 1; -1; 0; 1; max_int - 1; max_int ];
  check
    (Bigint.to_int_opt (Bigint.add (Bigint.of_int max_int) Bigint.one)
     = None);
  check
    (Bigint.to_int_opt (Bigint.sub (Bigint.of_int min_int) Bigint.one)
     = None)
;;

let check_primitive_order_is_distinct () =
  let small = Bigint.of_int 2 in
  let large = Bigint.of_string "2147483649" in
  check (not (small < large));
  check (Bigint.lt small large)
;;

let check_deep_values () =
  let nines = String.make 4_000 '9' in
  let value = Bigint.of_string nines in
  check (String.equal (Bigint.to_string value) nines);
  let square = Bigint.mul value value in
  check (String.length (Bigint.to_string square) = 8_000)
;;

let check_python_oracle () =
  if Sys.command "python3 -c ''" <> 0
  then ()
  else begin
    let script = "bigint_oracle.py" in
    let input = "bigint_oracle.in" in
    let output = "bigint_oracle.out" in
    let values =
      List.init 100 (fun _ -> random_decimal 120, random_decimal 120)
    in
    Fun.protect
      ~finally:(fun () ->
        List.iter
          (fun file -> try Sys.remove file with Sys_error _ -> ())
          [ script; input; output ])
      (fun () ->
        let channel = open_out script in
        output_string channel
          "import sys\nfor line in sys.stdin:\n a,b=map(int,line.split())\n print(a+b,a-b,a*b,(a>b)-(a<b),-a,abs(a))\n";
        close_out channel;
        let channel = open_out input in
        List.iter
          (fun (left, right) ->
            Printf.fprintf channel "%s %s\n" left right)
          values;
        close_out channel;
        let command =
          Printf.sprintf
            "python3 %s < %s > %s"
            (Filename.quote script)
            (Filename.quote input)
            (Filename.quote output)
        in
        if Sys.command command <> 0 then failwith "Bigint oracle failed";
        let channel = open_in output in
        List.iter
          (fun (left_string, right_string) ->
            let actual = input_line channel in
            let left = Bigint.of_string left_string in
            let right = Bigint.of_string right_string in
            let expected =
              String.concat " "
                [ Bigint.to_string (Bigint.add left right)
                ; Bigint.to_string (Bigint.sub left right)
                ; Bigint.to_string (Bigint.mul left right)
                ; string_of_int (Bigint.compare left right)
                ; Bigint.to_string (Bigint.neg left)
                ; Bigint.to_string (Bigint.abs left)
                ]
            in
            if not (String.equal actual expected)
            then failwith "Bigint oracle mismatch")
          values;
        (match input_line channel with
         | _ -> failwith "Bigint oracle returned extra output"
         | exception End_of_file -> ());
        close_in channel)
  end
;;

let () =
  List.iter check_string boundaries;
  List.iter check_invalid invalid_strings;
  check_machine_boundaries ();
  check_primitive_order_is_distinct ();
  check_properties ();
  check_deep_values ();
  check_python_oracle ();
  Printf.printf "Bigint: %d checks passed\n" !checks
;;
