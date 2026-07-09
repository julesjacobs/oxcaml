(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/pvghost.mli ../lib/pvghost.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Prophecies WITHOUT the pv projection: [type 'a proph : value
   refines ('a)] declares that a prophecy denotes an 'a, so facts are
   stated about the prophecy value directly -- [unit{ p = v }],
   [sorted p] -- at whatever sort 'a instantiates to.  One type covers
   borrow_lib's int prophecies and slice_lib's list prophecies; this
   also pins the interface-side on-sight registration (the .mli block
   names Vox_Pvghost_ilist, which no exported refinement mentions).

   [new_proph] takes an inhabitation WITNESS (RustHorn: a prophecy is
   born from a live value), so a prophecy over an uninhabited sort
   cannot be minted -- see mechanics/lean_pvghost_witness.ml.  *)

open Pvghost

let use_list () : ilist{ sorted _ } =
  let w = Nil in
  let p = new_proph w in
  let v = Cons (1, Cons (2, Nil)) in
  let () = resolve p v in
  v

let use_int (n : int{ 0 <= _ }) : int{ 0 <= _ } =
  let p = new_proph n in
  let () = resolve p n in
  n
