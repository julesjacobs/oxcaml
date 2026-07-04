(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/pslice.mli ../lib/pslice.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the POLYMORPHIC slices (lib/pslice) at two element types
   in one module.  The generic list theory ([plen]/[pelem]/[pupd]/
   [pconst] and its lemmas) was proved ONCE, polymorphically, in
   pslice's interface; here it discharges an in-place swap over a
   FRUIT array (boxed elements, so reads globalize via [gbl]) and a
   fresh-array read at INT (which mode-crosses on its own).  The only
   per-type obligation is the [Inhabited] instance for the client's
   own datatype, one line in the block. *)

open Pslice

type fruit = Apple | Pear of int

[%%vox.lean {lean|
instance : Inhabited Vox_Lean_pslice_fruit := ⟨.Apple⟩
|lean}]

(* In-place swap of cells [i] and [j]: the residual's ghost is the
   two-point update, and the width survives by [plen_pupd]. *)
let swap :
  (x : fruit varr{ plen (pcts _) = 4 }) @ unique ->
  (i : int{ 0 <= _ && _ < 4 }) -> (j : int{ 0 <= _ && _ < 4 }) ->
  fruit varr{ pcts _ = pupd (pupd (pcts x) i (pelem (pcts x) j)) j (pelem (pcts x) i)
              && plen (pcts _) = 4 } @ unique =
  fun x i j ->
    let p = new_proph () in
    let (x', u) =
      borrow p x (fun m ->
        let (a, m1) = sget m i in
        let a = gbl a in
        let (b, m2) = sget m1 j in
        let b = gbl b in
        let m3 = sset m2 i b in
        let m4 = sset m3 j a in
        let _u = sdrop m4 in
        (() : unit{ ppv p = pupd (pupd (pcts x) i (pelem (pcts x) j)) j (pelem (pcts x) i) }))
    in
    ignore u;
    (x' : fruit varr{ pcts _ = pupd (pupd (pcts x) i (pelem (pcts x) j)) j (pelem (pcts x) i)
                      && plen (pcts _) = 4 })

(* The int instantiation: a fresh array is constant ([pconst]), so
   cell 0 reads back the fill value by [pelem_pconst]. *)
let read_fresh : (n : int{ 0 < _ }) -> (v : int) -> int{ _ = v } =
  fun n v ->
    let x = pnew n v in
    let p = new_proph () in
    let (x', r) =
      borrow p x (fun m ->
        let (a, m1) = sget m 0 in
        let _u = sdrop m1 in
        (refine_ a : int{ _ = v && ppv p = pcts x }))
    in
    ignore x';
    (refine_ r : int{ _ = v })
