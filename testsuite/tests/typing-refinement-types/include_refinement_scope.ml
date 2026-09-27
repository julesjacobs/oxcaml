(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Values bound by [include] or [open struct ... end] are in scope for the
   rest of the structure, so refinements can mention them. *)

module Inc0 = struct let (f @ total) (x : int) = x + 1 end;;
[%%expect{|
module Inc0 : sig val f : int -> int end
|}]

module Included = struct
  include Inc0
  let p (e : int) : {u : unit | f e = f e} = ()
end;;
[%%expect{|
module Included :
  sig val f : int -> int val p : (e : int) -> {u : unit | (f e) = (f e)} end
|}]

module Recursive = struct
  include Inc0
  let rec (p @ total) : (e : int) -> {u : unit | f e = f e} = fun e -> ()
end;;
[%%expect{|
module Recursive :
  sig val f : int -> int val p : (e : int) -> {u : unit | (f e) = (f e)} end
|}]

module type Included_sig = sig
  include module type of Inc0
  val p : (e : int) -> {u : unit | f e = f e}
end;;
[%%expect{|
module type Included_sig =
  sig
    val f : int -> int @@ total
    val p : (e : int) -> {u : unit | (f e) = (f e)}
  end
|}]

(* A value hidden by [open struct] cannot appear in the signature; this used
   to be a fatal error for a recursive binding. *)
module Opened = struct
  open struct let (g @ total) (x : int) = x * 2 end
  let rec (p @ total) : (e : int) -> {u : unit | g e = g e} = fun e -> ()
end;;
[%%expect{|
Line 2, characters 2-51:
2 |   open struct let (g @ total) (x : int) = x * 2 end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "g" introduced by this open appears in the signature.
Line 3, characters 11-12:
3 |   let rec (p @ total) : (e : int) -> {u : unit | g e = g e} = fun e -> ()
               ^
  The value "p" has no valid type if "g" is hidden.
|}]

(* A local [open struct] still cannot let a refinement escape. *)
let escapes () =
  let open struct let x = 1 end in
  let p : (e : int) -> {u : unit | e = x} = fun e -> () in
  p;;
[%%expect{|
Line 4, characters 2-3:
4 |   p;;
      ^
Error: the refinement type of this expression escapes the scope of binding "x"
Hint: bind "x" outside this expression.
|}]
