(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/ia_lib.mli ../lib/ia_lib.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The performance unlock: reads go through [Ia_lib.unsafe_get] -- no
   bounds check in the compiled code -- because its contract demands
   the proof at every call site instead.  The loops below discharge it
   from the loop bound and the path fact, so the stdlib's per-access
   check is deleted with nothing assumed about any index.  The array
   is the BUILT-IN theory ([Iarray.length] reflects; the unsafe read
   itself stays out of the logic -- only its contract matters), so
   NOTHING in this file is assumed.  docs/vox/bench compiles THIS
   file natively, under verification, and measures it against the
   bounds-checked loop. *)

let sum : (a : int iarray) -> int =
  fun a ->
    let n = Iarray.length a in
    let rec go : (i : int{ 0 <= _ && _ <= n }) -> int -> int =
      fun i acc -> if i < n then go (i + 1) (acc + Ia_lib.unsafe_get a i) else acc
    in
    go 0 0

(* Two arrays, one bound: the second array's contract carries the
   cross-array requirement, so ONE length read covers both reads in
   the loop. *)
let dot : (a : int iarray)
          -> (b : int iarray{ Iarray.length a <= Iarray.length _ }) -> int =
  fun a b ->
    let n = Iarray.length a in
    let rec go : (i : int{ 0 <= _ && _ <= n }) -> int -> int =
      fun i acc ->
        if i < n then go (i + 1) (acc + (Ia_lib.unsafe_get a i * Ia_lib.unsafe_get b i)) else acc
    in
    go 0 0
