(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* ADVERSARIAL (via): TRUSTED-GHOST edges.  A ghost sort is rendered
   VERBATIM and trusted (the declaring module asserts its facts).  The
   only reserved-name check bars the emitter's own [Vox_]/[v_] prefixes;
   naming a Lean universe/proposition ([Prop], [Type], [False]) is
   ALLOWED and does not explode -- these are the library's own trusted
   interpretation.  A spec function stated over a [Prop]-sorted ghost
   verifies coherently. *)

type p [@@vox.sort lean "Prop"]
type ty [@@vox.sort lean "Type"]
type fls [@@vox.sort lean "False"]
[%%expect{|
type p
type ty
type fls
|}]

[%%vox.lean {lean|
@[grind] def holds : Prop -> Prop := fun q => q
|lean}]

let via_prop : (x : p{ holds _ }) -> unit{ holds x } = fun x -> ()
[%%expect{|
val via_prop : (x : p{ holds _ }) -> unit{ holds x } = <fun>
|}]
