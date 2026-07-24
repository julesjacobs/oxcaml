(* TEST
 {
   flags = "-keywords 5.3 -vox-backend z3 -vox-dump-vc-json z3-vcs.json";
   expect;
 } {
   flags = "-keywords 5.3 -vox-backend lean -vox-dump-vc-json lean-vcs.json";
   expect;
 } {
   flags = "-keywords 5.3 -vox-backend oxsmt -vox-dump-vc-json oxsmt-vcs.json";
   expect;
 }
*)

let add_zero (value : Bigint.t @ logical)
    : unit{ Bigint.equal (Bigint.add value Bigint.zero) value } =
  ()
;;

let subtract_self (value : Bigint.t @ logical)
    : unit{ Bigint.is_zero (Bigint.sub value value) } =
  ()
;;

let negate_order (left : Bigint.t @ logical)
    (right : Bigint.t @ logical)
    (ordered : unit{ Bigint.lt left right })
    : unit{ Bigint.gt (Bigint.neg left) (Bigint.neg right) } =
  ordered
;;

let absolute_nonnegative (value : Bigint.t @ logical)
    : unit{ Bigint.ge (Bigint.abs value) Bigint.zero } =
  ()
;;

let multiplication_zero (value : Bigint.t @ logical)
    : unit{ Bigint.equal (Bigint.mul value Bigint.zero) Bigint.zero } =
  ()
;;

let compare_characterization (left : Bigint.t @ logical)
    (right : Bigint.t @ logical)
    : unit{
        Bigint.compare left right
        = if Bigint.lt left right
          then -1
          else if Bigint.gt left right then 1 else 0
      } =
  ()
;;

let polymorphic_equality_matches (left : Bigint.t @ logical)
    (right : Bigint.t @ logical)
    : unit{ (left = right) = Bigint.equal left right } =
  ()
;;

let conversion_constants
    : unit{
        Bigint.equal
          (Bigint.of_int 7)
          (Bigint.add (Bigint.of_int 3) (Bigint.of_int 4))
        && Bigint.le Bigint.zero Bigint.one
      } =
  ()
;;

module B = Bigint

let alias_add_zero (value : B.t @ logical)
    : unit{ B.equal (B.add value B.zero) value } =
  ()
;;

[%%expect{|
val add_zero :
  (value : Bigint.t) @ logical ->
  unit{ Bigint.equal (Bigint.add value Bigint.zero) value } = <fun>
val subtract_self :
  (value : Bigint.t) @ logical ->
  unit{ Bigint.is_zero (Bigint.sub value value) } = <fun>
val negate_order :
  (left : Bigint.t) @ logical ->
  (right : Bigint.t) @ logical ->
  unit{ Bigint.lt left right } ->
  unit{ Bigint.gt (Bigint.neg left) (Bigint.neg right) } = <fun>
val absolute_nonnegative :
  (value : Bigint.t) @ logical ->
  unit{ Bigint.ge (Bigint.abs value) Bigint.zero } = <fun>
val multiplication_zero :
  (value : Bigint.t) @ logical ->
  unit{ Bigint.equal (Bigint.mul value Bigint.zero) Bigint.zero } = <fun>
val compare_characterization :
  (left : Bigint.t) @ logical ->
  (right : Bigint.t) @ logical ->
  unit{
   Bigint.compare left right = (if Bigint.lt left right then (-1) else (if Bigint.gt left right then 1 else 0))
   } =
  <fun>
val polymorphic_equality_matches :
  (left : Bigint.t) @ logical ->
  (right : Bigint.t) @ logical ->
  unit{ left = right = Bigint.equal left right } = <fun>
val conversion_constants :
  unit{
   Bigint.equal (Bigint.of_int 7) (Bigint.add (Bigint.of_int 3) (Bigint.of_int 4)) && Bigint.le Bigint.zero Bigint.one
   } =
  ()
module B = Bigint
val alias_add_zero :
  (value : B.t) @ logical -> unit{ B.equal (B.add value B.zero) value } =
  <fun>
|}]
