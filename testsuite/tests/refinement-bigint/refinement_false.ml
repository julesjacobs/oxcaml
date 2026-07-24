(* TEST
 flags = "-keywords 5.3 -vox-backend z3";
 expect;
*)

let false_addition (value : Bigint.t @ logical)
    : unit{
        Bigint.equal
          (Bigint.add value Bigint.one)
          value
      } =
  ()
;;

[%%expect{|
Line 7, characters 2-4:
7 |   ()
      ^^
Error: Refinement verification failed (disproved)
|}]

let false_compare
    : unit{ Bigint.compare Bigint.zero Bigint.one = 0 } =
  ()
;;

[%%expect{|
Lines 1-3, characters 4-4:
1 | ....false_compare
2 |     : unit{ Bigint.compare Bigint.zero Bigint.one = 0 } =
3 |   ()
Error: Refinement verification failed (disproved)
|}]
