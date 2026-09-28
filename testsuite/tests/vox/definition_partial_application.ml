(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A partial application of a [@def] function is total, whichever use is
   typed first. *)

module Full_first = struct
  let[@def] (post2 @ total) (k : int) (x : int) = x > k
  let (apply @ total) (p : (int -> bool) @ total) (x : int) : unit = ()
  let (use_full @ total) () = post2 1 3
  let (use_partial @ total) () = apply (post2 1) 3
end;;
[%%expect{|
module Full_first :
  sig
    val post2 : int @ total immutable -> (int -> bool) @ total
    val post2_def :
      (k : int) @ immutable ->
      (x : int) -> {u : unit | (post2 k x) === (x > k)}
    val apply : (int -> bool) @ total -> int -> unit
    val use_full : unit -> bool
    val use_partial : unit -> unit
  end
|}]

module Partial_first = struct
  let[@def] (post2 @ total) (k : int) (x : int) = x > k
  let (apply @ total) (p : (int -> bool) @ total) (x : int) : unit = ()
  let (use_partial @ total) () = apply (post2 1) 3
  let (use_full @ total) () = post2 1 3
end;;
[%%expect{|
module Partial_first :
  sig
    val post2 : int @ total immutable -> (int -> bool) @ total
    val post2_def :
      (k : int) @ immutable ->
      (x : int) -> {u : unit | (post2 k x) === (x > k)}
    val apply : (int -> bool) @ total -> int -> unit
    val use_partial : unit -> unit
    val use_full : unit -> bool
  end
|}]
