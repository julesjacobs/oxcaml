(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* ADVERSARIAL (via): EXOTIC SKELETONS and LAYERING.  The [@vox.via]
   attribute wraps ANY skeleton as [Trefine(skel, [map], true)] -- it is
   never silently dropped.  On semantically-degenerate skeletons (arrow,
   mutable record, GADT, a bare type variable) it attaches without
   crashing; the map is trusted and never applied to a real value of
   such a type.  The DOCUMENTED way to stack abstractions is nested type
   aliases, which composes maps in the right order and denotes at the
   final image. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]
type ibag [@@vox.sort lean "IBag"]

[%%vox.lean {lean|
inductive ISet where | nil : ISet | cons : Int -> ISet -> ISet
inductive IBag where | bnil : IBag | bcons : Int -> IBag -> IBag
@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True | .Node l v r => bst l ∧ bst r
@[grind] def elems : Vox_tree -> ISet
  | .Leaf => .nil | .Node l v _ => .cons v (elems l)
@[grind] def toBag : ISet -> IBag
  | .nil => .bnil | .cons x s => .bcons x (toBag s)
@[grind] def bmem (x : Int) : IBag -> Prop
  | .bnil => False | .bcons y s => x = y ∨ bmem x s
|lean}]

(* via on exotic skeletons: each attaches without crashing. *)
type f_arrow = (int -> int) [@vox.via (elems : iset)]
type rrec = { mutable m : int }
type f_mutrec = rrec [@vox.via (elems : iset)]
type _ gadt = GI : int -> int gadt
type f_gadt = int gadt [@vox.via (elems : iset)]
type 'a f_tvar = 'a [@vox.via (elems : iset)]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
type ibag
type f_arrow = (int -> int){ true via (elems : iset) }
type rrec = { mutable m : int; }
type f_mutrec = rrec{ true via (elems : iset) }
type _ gadt = GI : int -> int gadt
type f_gadt = int gadt{ true via (elems : iset) }
type 'a f_tvar = 'a{ true via (elems : iset) }
|}]

(* DOCUMENTED layering via nested aliases: [set] denotes at iset, [bag]
   at ibag; a binder of [bag] speaks the ibag vocabulary directly. *)
type set = tree{ bst _ } [@vox.via (elems : iset)]
type bag = set [@vox.via (toBag : ibag)]
[%%expect{|
type set = tree{ bst _ via (elems : iset) }
type bag = set{ true via (toBag : ibag) }
|}]

let layered : (t : bag{ bmem 0 _ }) -> unit{ bmem 0 t } = fun t -> ()
[%%expect{|
val layered :
  (t : set{ true && bmem 0 (toBag _) via (toBag : ibag) }) ->
  unit{ bmem 0 t } = <fun>
|}]
