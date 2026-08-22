(* TEST
 {
   reference = "${test_source_directory}/ghost_runtime.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/ghost_runtime.reference";
   native;
 }
*)

(* Runtime semantics of ghostliness. The mode has no ABI effect: ghost
   arguments are passed physically (as placeholders when built with
   [ghost_]), and functions with ghost parameters have ordinary calling
   conventions.

   The ghost expression's effects are gone. This pins the deliberate
   unsoundness of ghost_ before the totality piece requires e @ total: if
   deletion of effects ever changes, this test's reference must change with
   it. *)

let f (x : int @ ghost) (n : int) (m : int) = n + m

let g (u : unit) (z : int @ ghost) = print_string "g ran\n"

let () =
  (* ghost_ deletes evaluation, including effects and exceptions *)
  let x = ghost_ (print_string "DELETED\n"; failwith "never") in
  g () x;
  (* a ghost argument built with ghost_ is never evaluated *)
  g () (ghost_ (print_string "ALSO DELETED\n"; 6));
  (* a real argument at a ghost parameter is evaluated and passed
     like any argument; the callee just cannot read it *)
  g () (print_string "kept effect\n"; 7);
  (* partial application across a ghost parameter *)
  let h = f x 10 in
  print_int (h 20); print_newline ();
  (* a function whose only parameter is ghost *)
  let use (y : int @ ghost) = f y 1 in
  print_int (use x 2); print_newline ();
  (* a real closure capturing a ghost value is an ordinary closure *)
  let clo = fun u -> g u x in
  clo ();
  (* an alias of a ghost variable is itself ghost *)
  let x' = x in
  g () x';
  (* ghost values may flow through branches and closures; the results are
     only usable at ghost positions *)
  let w = if Sys.opaque_identity true then x else x' in
  g () w;
  let k = fun () -> x in
  g () (k ());
  (* labelled and out-of-order application across a ghost parameter *)
  let lab ~(a : int @ ghost) ~(b : int) = b + 1 in
  print_int (lab ~b:1 ~a:x);
  print_newline ();
  (* a ghost parameter of a function-cases function *)
  let fc : int @ ghost -> int = function _ -> 3 in
  print_int (fc x);
  print_newline ();
  (* ghost optional parameters, defaulted and not, are passed as options
     like any optional *)
  let opt ?a:(_ : int option @ ghost) () = 4 in
  print_int (opt ());
  print_int (opt ~a:5 ());
  print_int (opt ?a:(Some (ghost_ 6)) ());
  print_newline ();
  print_string "done\n"
