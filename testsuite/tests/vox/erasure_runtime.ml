(* TEST
 {
   reference = "${test_source_directory}/erasure_runtime.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/erasure_runtime.reference";
   native;
 }
*)

(* Runtime semantics of erasure. The mode has no ABI effect: erased
   arguments are passed physically (as placeholders when built with
   [erased_]), and functions with erased parameters have ordinary calling
   conventions.

   The erased expression's effects are gone. This pins the deliberate
   unsoundness of erased_ before the totality piece requires e @ total: if
   deletion of effects ever changes, this test's reference must change with
   it. *)

let f (x : int @ erased) (n : int) (m : int) = n + m

let g (u : unit) (z : int @ erased) = print_string "g ran\n"

let () =
  (* erased_ deletes evaluation, including effects and exceptions *)
  let x = erased_ (print_string "DELETED\n"; failwith "never") in
  g () x;
  (* an erased argument built with erased_ is never evaluated *)
  g () (erased_ (print_string "ALSO DELETED\n"; 6));
  (* a retained argument at an erased parameter is evaluated and passed
     like any argument; the callee just cannot read it *)
  g () (print_string "kept effect\n"; 7);
  (* partial application across an erased parameter *)
  let h = f x 10 in
  print_int (h 20); print_newline ();
  (* a function whose only parameter is erased *)
  let use (y : int @ erased) = f y 1 in
  print_int (use x 2); print_newline ();
  (* a retained closure capturing an erased value is an ordinary closure *)
  let clo = fun u -> g u x in
  clo ();
  (* an alias of an erased variable is itself erased *)
  let x' = x in
  g () x';
  (* erased values may flow through branches and closures; the results are
     only usable at erased positions *)
  let w = if Sys.opaque_identity true then x else x' in
  g () w;
  let k = fun () -> x in
  g () (k ());
  (* labelled and out-of-order application across an erased parameter *)
  let lab ~(a : int @ erased) ~(b : int) = b + 1 in
  print_int (lab ~b:1 ~a:x);
  print_newline ();
  (* an erased parameter of a function-cases function *)
  let fc : int @ erased -> int = function _ -> 3 in
  print_int (fc x);
  print_newline ();
  (* erased optional parameters, defaulted and not, are passed as options
     like any optional *)
  let opt ?a:(_ : int option @ erased) () = 4 in
  print_int (opt ());
  print_int (opt ~a:5 ());
  print_int (opt ?a:(Some (erased_ 6)) ());
  print_newline ();
  print_string "done\n"
