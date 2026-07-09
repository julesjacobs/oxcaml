type 'a ord [@@vox.sort lean "POrd"]
[%%vox.lean {lean|
@[grind, expose] def POrd (a : Type) : Type := a -> a -> Prop
@[grind, expose] def ple {a : Type} (o : POrd a) (x y : a) : Prop := o x y
|lean}]
(* result refinement mentions the OTHER 'a arg a *)
let pick : (o : 'a ord) -> (a : 'a) -> 'a{ ple o a _ } =
  fun _o a -> a
