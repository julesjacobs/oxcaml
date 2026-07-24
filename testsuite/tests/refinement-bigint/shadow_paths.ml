(* TEST
 flags = "-keywords 5.3 -vox-backend z3";
 expect;
*)

module Stdlib__Bigint = struct
  type t = int

  let zero = 0
  let one = 1
  let add @ total = fun _left _right -> 0
  let compare @ total = fun _left _right -> 0
  external equal : t -> t -> bool @@ total = "%equal"
end

let local_flat_name (value : Stdlib__Bigint.t @ logical)
    : unit{
        Stdlib__Bigint.equal
          (Stdlib__Bigint.add value Stdlib__Bigint.zero)
          value
      } =
  ()
;;

[%%expect{|
module Stdlib__Bigint :
  sig
    type t = int
    val zero : int
    val one : int
    val add : 'a @ total logical -> 'b -> int
    val compare : 'a @ total logical -> 'b -> int
    external equal : t -> t -> bool = "%equal"
  end
Line 17, characters 2-4:
17 |   ()
       ^^
Error: Refinement verification failed (not-proved)
|}]

let local_flat_compare
    : unit{
        Stdlib__Bigint.compare
          Stdlib__Bigint.zero
          Stdlib__Bigint.one
        = -1
      } =
  ()
;;

[%%expect{|
Lines 1-8, characters 4-4:
1 | ....local_flat_compare
2 |     : unit{
3 |         Stdlib__Bigint.compare
4 |           Stdlib__Bigint.zero
5 |           Stdlib__Bigint.one
6 |         = -1
7 |       } =
8 |   ()
Error: Refinement verification failed (not-proved)
|}]

module Stdlib = struct
  module Bigint = struct
    type t = int

    let zero = 0
    let one = 1
    let add @ total = fun _left _right -> 0
    let compare @ total = fun _left _right -> 0
    external equal : t -> t -> bool @@ total = "%equal"
  end
end

let local_nested_name (value : Stdlib.Bigint.t @ logical)
    : unit{
        Stdlib.Bigint.equal
          (Stdlib.Bigint.add value Stdlib.Bigint.zero)
          value
      } =
  ()
;;

[%%expect{|
module Stdlib :
  sig
    module Bigint :
      sig
        type t = int
        val zero : int
        val one : int
        val add : 'a @ total logical -> 'b -> int
        val compare : 'a @ total logical -> 'b -> int
        external equal : t -> t -> bool = "%equal"
      end
  end
Line 19, characters 2-4:
19 |   ()
       ^^
Error: Refinement verification failed (not-proved)
|}]

let local_nested_compare
    : unit{
        Stdlib.Bigint.compare Stdlib.Bigint.zero Stdlib.Bigint.one = -1
      } =
  ()
;;

[%%expect{|
Lines 1-5, characters 4-4:
1 | ....local_nested_compare
2 |     : unit{
3 |         Stdlib.Bigint.compare Stdlib.Bigint.zero Stdlib.Bigint.one = -1
4 |       } =
5 |   ()
Error: Refinement verification failed (not-proved)
|}]
