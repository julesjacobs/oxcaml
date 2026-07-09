(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* vox: [assume_ e] compiles a runtime check of the refinement
   predicate, raising [Failure] when it does not hold;
   [assume_unchecked_ e] compiles to nothing. *)

let pos (n : int) : {v:int | v > 0} = assume_ n

let () =
  (* Passing check: the value flows through unchanged. *)
  let refine_ x = pos 5 in
  Printf.printf "pos 5 = %d\n" x;
  (* Failing check: Failure carries the location and predicate. *)
  (match pos (-3) with
   | _ -> print_endline "unreachable: check did not fire"
   | exception Failure msg -> Printf.printf "failure: %s\n" msg);
  (* Compound predicate, both polarities. *)
  let range (n : int) : {v:int | v >= 0 && v <= 9 && not (v = 5)} =
    assume_ n
  in
  let refine_ y = range 7 in
  Printf.printf "range 7 = %d\n" y;
  (match range 5 with
   | _ -> print_endline "unreachable: check did not fire"
   | exception Failure _ -> print_endline "range 5 rejected");
  (* The predicate may mention program variables in scope. *)
  let k = 10 in
  let above (n : int) : {v:int | v > k} = assume_ n in
  let refine_ z = above 11 in
  Printf.printf "above 11 = %d (k = %d)\n" z k;
  (match above 10 with
   | _ -> print_endline "unreachable: check did not fire"
   | exception Failure _ -> print_endline "above 10 rejected");
  (* Dependent-arrow binders are opened to the parameters' stamps, so
     userland operations get real checks. *)
  let mul (x : int) (y : int) : {z:int | z = x * y} = assume_ (x * y) in
  let a = 6 in
  let b = 7 in
  let refine_ m = mul a b in
  Printf.printf "mul 6 7 = %d\n" m;
  (* A dependent check that lies fails at runtime. *)
  let bad_mul (x : int) (y : int) : {z:int | z = x * y} =
    assume_ (x * y + 1)
  in
  (match bad_mul a b with
   | _ -> print_endline "unreachable: check did not fire"
   | exception Failure _ -> print_endline "bad_mul rejected");
  (* Compact syntax works the same. *)
  let compact (n : int) : int{ _ >= 100 } = assume_ n in
  let refine_ c = compact 100 in
  Printf.printf "compact 100 = %d\n" c;
  (* assume_unchecked_ performs no check: the lie goes through. *)
  let unchecked : {v:int | v > 100} = assume_unchecked_ 1 in
  Printf.printf "unchecked = %d\n" unchecked
