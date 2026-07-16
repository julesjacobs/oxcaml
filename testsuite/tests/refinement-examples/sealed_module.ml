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
   refined; FINAL sealing proves the directed implication.  The Seals merge made
   the seal-VC engage here, so rejection is now the directed-implication VC
   (structural mismatch -> not-proved), as this example predicted; the VC still
   awaits total-comparisons to reach the final ACCEPT. *)

#load "vox_spec.cmo";;

(* @ex id=seal_square_nonnegative final=ACCEPT today=REJECT stable=no unlocks=total-comparisons+verification *)
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
Error: Refinement verification failed at module seal for value "square" (not-proved)
Line 2, characters 2-48:
2 |   val square : int -> int{ Vox_spec.int_ge _ 0 }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value square
Line 4, characters 6-12:
4 |   let square (x : int) = (x * x : int{ _ = x * x })
          ^^^^^^
  Implementation declaration for value square
|}]
