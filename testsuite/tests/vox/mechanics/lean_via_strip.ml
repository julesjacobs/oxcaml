(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Image-binder made three typecore strip sites branch on maps=[] vs
   via: an ORDINARY refinement ([maps = []]) still strips to its
   skeleton at a use exactly as before; only a VIA type ([maps <> []])
   is kept (reached through [refine_]).  This test pins "strip
   refinements, never maps" at each seam so a future rebase through
   typecore cannot silently regress ordinary refinements. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where | nil : ISet | cons : Int -> ISet -> ISet
@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l v r => bst l ∧ bst r
@[grind] def elems : Vox_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
|lean}]

type set = tree{ bst _ } [@vox.via (elems : iset)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type set = tree{ bst _ via (elems : iset) }
|}]

(* SEAM 1 -- param binding (typecore [vox_is_param_root]).  An ordinary
   [int{ _ >= 0 }] param strips to [int], so it is usable directly as
   an int, and its binder fact [n >= 0] discharges the result. *)
let strip_param : (n : int{ _ >= 0 }) -> int{ _ >= 0 } =
  fun n -> n + 0
[%%expect{|
val strip_param : int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]

(* SEAM 1 (datatype refinement) -- an ordinary [tree{ bst _ }] param
   strips to [tree], so it is matchable directly; the binder fact
   [bst t] holds. *)
let strip_tree : (t : tree{ bst _ }) -> unit{ bst t } =
  fun t -> match t with Leaf -> () | Node (_, _, _) -> ()
[%%expect{|
val strip_tree : (t : tree{ bst _ }) -> unit{ bst t } = <fun>
|}]

(* SEAM 2 -- value reference (typecore use_ty).  A let-bound ordinary
   refined value strips to its skeleton at the use. *)
let strip_let : unit -> int{ _ >= 0 } =
  fun () ->
    let r : int{ _ >= 0 } = assume_unchecked_ 5 in
    r + 0
[%%expect{|
val strip_let : unit -> int{ _ >= 0 } = <fun>
|}]

(* SEAM 3 -- param-refinement walker ([vox_strip_param_refinement], the
   application-argument path).  Passing an ordinary refined value as an
   argument strips it, and the callee's precondition [n >= 0] is
   discharged from the argument's binder fact. *)
let strip_arg : (n : int{ _ >= 0 }) -> int{ _ >= 0 } =
  fun n -> strip_param n
[%%expect{|
val strip_arg : int{ _ >= 0 } -> int{ _ >= 0 } = <fun>
|}]
