(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* ADVERSARIAL (via): CYCLIC via targets.  The refines feature needed
   cycle-termination hardening; via targets are the sibling.  Three
   shapes: (1) a via whose target IS itself, (2) two vias whose targets
   are each other, (3) a cycle that runs through a DATATYPE target (the
   recursive-abbreviation checker permits this, so sort resolution's
   [visited] guard is what must terminate).  Verdict: (1)/(2) are caught
   cleanly by OCaml's own recursive-abbreviation checker BEFORE any sort
   resolution; (3) terminates (the [dsort_of_type]/[register_type_specs]
   visited guards fire, degrading to the uninterpreted sort). *)

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

(* (1) self-referential via: the target is the type being defined. *)
type selfvia = tree{ bst _ } [@vox.via (elems : selfvia)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
Line 15, characters 0-57:
15 | type selfvia = tree{ bst _ } [@vox.via (elems : selfvia)]
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "selfvia" is cyclic:
         "selfvia" = "tree{ bst _ via (elems : selfvia) }",
         "tree{ bst _ via (elems : selfvia) }" contains "selfvia"
|}]

(* (2) mutually-recursive via targets. *)
type a = tree{ bst _ } [@vox.via (fa : b)]
and  b = tree{ bst _ } [@vox.via (fb : a)]
[%%expect{|
Line 1, characters 0-42:
1 | type a = tree{ bst _ } [@vox.via (fa : b)]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "a" is cyclic:
         "a" = "tree{ bst _ via (fa : b) }",
         "tree{ bst _ via (fa : b) }" contains "b",
         "b" = "tree{ bst _ via (fb : a) }",
         "tree{ bst _ via (fb : a) }" contains "a"
|}]

(* (3) cycle through a DATATYPE target: [w] is a via type whose target
   is datatype [box]; [box]'s field is [w].  Sort resolution must
   terminate. *)
type box = Box of w
and w = tree{ bst _ } [@vox.via (f : box)]
[%%expect{|
type box = Box of w
and w = tree{ bst _ via (f : box) }
|}]

let use_w : (t : w) -> unit = fun _ -> ()
[%%expect{|
val use_w : w -> unit = <fun>
|}]
