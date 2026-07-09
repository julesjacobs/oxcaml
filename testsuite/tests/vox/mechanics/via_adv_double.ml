(* TEST
 expect;
*)

(* ADVERSARIAL (via) -- F1 fix: two [@vox.via] attributes on ONE type are
   REJECTED.  Composing them on a single skeleton would chain the maps in
   attribute-decode order rather than sort order (the map codomains would
   not line up), emitting ill-typed solver input.  The supported way to
   stack abstractions is nested type aliases (see via_adv_skeletons.ml:
   [type set = tree{..} via elems] then [type bag = set via toBag]).  The
   error fires at type elaboration -- no solver needed. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]
type ibag [@@vox.sort lean "IBag"]

type bag = tree{ true } [@vox.via (elems : iset)] [@vox.via (toBag : ibag)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type ibag
Line 5, characters 24-49:
5 | type bag = tree{ true } [@vox.via (elems : iset)] [@vox.via (toBag : ibag)]
                            ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: a type may carry at most one [@vox.via] attribute; layer abstractions through nested type aliases instead
|}]
