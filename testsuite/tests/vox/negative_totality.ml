(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Negative = struct
  type t = Roll of (t -> int)
  let (self_apply @ total) (x : t @ total) =
    match x with Roll f -> f x
  let (loop @ total) () = self_apply (Roll self_apply)
end;;
[%%expect{|
Line 4, characters 17-23:
4 |     match x with Roll f -> f x
                     ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 3-4, characters 27-30
         which is expected to be "total".
|}]

module False_proof = struct
  type t = Roll of (t @ total -> {u : unit | false}) @@ total
  let (self_apply @ total) (x : t @ total) : {u : unit | false} =
    match x with Roll f -> f x
  let (contradiction @ total) () = ghost_ (self_apply (Roll self_apply))
end;;
[%%expect{|
Line 4, characters 17-23:
4 |     match x with Roll f -> f x
                     ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 3-4, characters 27-30
         which is expected to be "total".
|}]

module Negative_record = struct
  type t = { run : t @ total -> {u : unit | false} }
  let (self_apply @ total) (x : t @ total) = x.run x
  let (contradiction @ total) () = ghost_ (self_apply {run = self_apply})
end;;
[%%expect{|
Line 3, characters 45-50:
3 |   let (self_apply @ total) (x : t @ total) = x.run x
                                                 ^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 27-52
         which is expected to be "total".
|}]

module Negative_alias = struct
  type 'a consumer = 'a @ total -> int
  type t = Roll of t consumer
  let (self_apply @ total) (x : t @ total) =
    let Roll f = x in f x
end;;
[%%expect{|
Line 5, characters 8-14:
5 |     let Roll f = x in f x
            ^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 4-5, characters 27-25
         which is expected to be "total".
|}]

module Ordinary_partial = struct
  type t = Roll of (t -> int)
  let self_apply x = match x with Roll f -> f x
  let loop () = self_apply (Roll self_apply)
end;;
[%%expect{|
module Ordinary_partial :
  sig
    type t = Roll of (t -> int)
    val self_apply : t -> int
    val loop : unit -> int
  end
|}]
