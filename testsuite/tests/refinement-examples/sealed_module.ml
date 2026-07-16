(* TEST
 modules = "vox_spec.ml";
 setup-ocamlc.byte-build-env;
 module = "vox_spec.ml";
 ocamlc.byte;
 module = "";
 flags += " -I ${test_build_directory}/ocamlc.byte";
 expect;
*)

(* This avoids the unresolved bare-implementation direction.  The
   implementation result [_ = x * x] and interface result [_ >= 0] are both
   refined; FINAL sealing proves the directed implication.  CURRENT rigid
   signature inclusion rejects the different predicates. *)

#load "vox_spec.cmo";;

(* @ex id=seal_square_nonnegative final=ACCEPT today=REJECT stable=no unlocks=total-comparisons+seals+verification *)
module Square : sig
  val square : int -> int{ Vox_spec.int_ge _ 0 }
end = struct
  let square (x : int) = (x * x : int{ _ = x * x })
end

[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let square (x : int) = (x * x : int{ _ = x * x })
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val square :
             int ->
             int{
              (app[Stdlib!.=] _ (app[Stdlib!.*] global[x/291] global[x/291]))
              }
         end
       is not included in
         sig val square : int -> int{ (app[Vox_spec!.int_ge] _ 0) } end
       Values do not match:
         val square :
           int ->
           int{
            (app[Stdlib!.=] _ (app[Stdlib!.*] global[x/291] global[x/291])) }
       is not included in
         val square : int -> int{ (app[Vox_spec!.int_ge] _ 0) }
       The type
         "int ->
         int{ (app[Stdlib!.=] _ (app[Stdlib!.*] global[x/291] global[x/291]))
          }"
       is not compatible with the type
         "int -> int{ (app[Vox_spec!.int_ge] _ 0) }"
       Type
         "int{ (app[Stdlib!.=] _ (app[Stdlib!.*] global[x/291] global[x/291]))
          }"
       is not compatible with type "int{ (app[Vox_spec!.int_ge] _ 0) }"
|}]
