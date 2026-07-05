(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* INLINE [@vox.via] positions (solverless).  The via attribute is
   decoded in [transl_type_aux] (typetexp.ml ~1349), so it is meant to
   work in ANY core-type position, not only on a [type] definition.
   This pins the inline spellings: arrow domain, arrow result, under a
   type constructor, and anonymous unification against the named form.
   The end-to-end PROOFS for the domain/result cases are in
   lean_via_inline.ml; here we pin decode + VC SHAPE + unification only.

   ONE SYNTACTIC RULE (discovered): an inline via in an arrow named
   binder must be PARENTHESISED -- [(t : (ty [@vox.via (m : s)]))] --
   because a refinement or attribute written directly after [t :] does
   not parse.  With that paren the attribute lands on [ty] and every
   TYPE-level use below works, identically to the named form.

   KNOWN VALUE-LEVEL GAP (inline only): reaching the SKELETON of an
   inline-via *value* diverges from the named form -- see the comment
   above [refine_inline_rejected] at the bottom. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind] def mem (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem x s

@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l v r => bst l ∧ bst r

@[grind] def elems : Vox_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
|lean}]
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
|}]

(* CASE 1 -- INLINE ARROW DOMAIN.  The inline via is decoded exactly as
   the named [type set = ...] would be: the binder DENOTES THE IMAGE
   (its refinement [mem 0 _] is at the image sort), so the goal and the
   binder fact are both the image-level [mem 0 t] -- no [elems] in
   sight.  Identical VC to lean_via.ml's [binder_image] on the named
   [set]. *)
let binder_image
  : (t : (tree{ bst _ } [@vox.via (elems : iset)]){ mem 0 _ }) -> unit{ mem 0 t } =
  fun t -> ()
[%%expect{|
Line 3, characters 11-13: vox VC:
  goal: mem 0 t
  hypotheses:
  mem 0 t
val binder_image :
  (t : tree{ (bst _) && (mem 0 (elems _)) via (elems : iset) }) ->
  unit{ mem 0 t } = <fun>
|}]

(* CASE 2 -- INLINE ARROW RESULT.  Constructing the result generates
   the IMAGE OBLIGATION, lowered through the map: the goal is [bst _ &&
   mem x (elems _)] on the built node.  (Assumed here; proved in
   lean_via_inline.ml.) *)
let mk
  : (x : int) -> (tree{ bst _ } [@vox.via (elems : iset)]){ mem x _ } =
  fun x -> assume_unchecked_ (Node (Leaf, x, Leaf))
[%%expect{|
Line 3, characters 29-51: vox VC (ASSUMED):
  goal: (bst (Node (Leaf, x, Leaf))) && (mem x (elems (Node (Leaf, x, Leaf))))
  hypotheses: <none>
val mk : (x : int) -> tree{ (bst _) && (mem x (elems _)) via (elems : iset) } =
  <fun>
|}]

(* CASE 3 -- UNDER A TYPE CONSTRUCTOR.  The via survives under [list]
   and under [iarray]; the element type keeps its via, so per the
   existing refinement-under-constructor behaviour an element reached
   from the collection carries the via element type (reached, as ever,
   through [refine_]).  We pin the type-level survival at both. *)
let under_list : ((tree{ bst _ } [@vox.via (elems : iset)]) list) -> unit =
  fun _ -> ()
[%%expect{|
val under_list : tree{ bst _ via (elems : iset) } list -> unit = <fun>
|}]

let under_iarray : ((tree{ bst _ } [@vox.via (elems : iset)]) iarray) -> unit =
  fun _ -> ()
[%%expect{|
val under_iarray : tree{ bst _ via (elems : iset) } iarray -> unit = <fun>
|}]

(* CASE 5 -- ANONYMOUS UNIFICATION WITH THE NAMED FORM.  A named [set]
   and the identical inline spelling UNIFY: rigid refinement
   unification compares skeleton + maps + predicate, and the named form
   is just an abbreviation of that same [Trefine].  So a value at [set]
   is accepted by a consumer written with the inline spelling.  (The
   [bst s] goal is the via's inherent base-predicate obligation -- a
   skeleton fact reached through [refine_] in real code -- assumed under
   dry-run; it is orthogonal to the unification being pinned here.) *)
type set = tree{ bst _ } [@vox.via (elems : iset)]
[%%expect{|
type set = tree{ bst _ via (elems : iset) }
|}]

let consume : (t : (tree{ bst _ } [@vox.via (elems : iset)])) -> unit =
  fun t -> ()
let bridge : (s : set) -> unit = fun s -> consume s
[%%expect{|
val consume : tree{ bst _ via (elems : iset) } -> unit = <fun>
Line 3, characters 50-51: vox VC:
  goal: bst s
  hypotheses: <none>
val bridge : set -> unit = <fun>
|}]

(* CASE 4 -- MISMATCH IS RIGIDLY REJECTED.  Two inline spellings whose
   MAP FUNCTIONS differ ([elems] vs [elems2]) do NOT unify: rigid
   unification of the maps fails -- a plain type clash, fail-closed, no
   solver involved. *)
[%%vox.lean {lean|
@[grind] def elems2 : Vox_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems2 l)
|lean}]
[%%expect{|
|}]

let produce2 : unit -> (tree{ bst _ } [@vox.via (elems : iset)]) =
  fun () -> assume_unchecked_ Leaf
let consume2 : (t : (tree{ bst _ } [@vox.via (elems2 : iset)])) -> unit =
  fun t -> ()
let bridge_mismatch : unit -> unit = fun () -> consume2 (produce2 ())
[%%expect{|
Line 2, characters 30-34: vox VC (ASSUMED):
  goal: bst Leaf
  hypotheses: <none>
val produce2 : unit -> tree{ bst _ via (elems : iset) } = <fun>
val consume2 : tree{ bst _ via (elems2 : iset) } -> unit = <fun>
Line 5, characters 56-69:
5 | let bridge_mismatch : unit -> unit = fun () -> consume2 (produce2 ())
                                                            ^^^^^^^^^^^^^
Error: This expression has type "tree{ bst _ via (elems : iset) }"
       but an expression was expected of type
         "tree{ bst _ via (elems2 : iset) }"
|}]

(* KNOWN VALUE-LEVEL GAP (inline only); the fail-CLOSED part is pinned
   below.

   The named form ([type set = tree{ bst _ } [@vox.via ...]]) is a
   [Tconstr] abbreviation; the inline spelling is a bare [Trefine]
   node.  The value-side via discipline -- the no-implicit-projection
   guard (typecore.ml:7813) and [refine_] recognition
   (typecore.ml:4088, which [expand_head]s the scrutinee and looks for
   a [Trefine]) -- is correct for the [Tconstr] form but NOT for a bare
   [Trefine] value: by the time an inline-via value is bound to a name
   and used, its maps are no longer seen, so it is treated like an
   ordinary refinement that STRIPS to the skeleton.  Consequences:

   (a) [refine_] on an inline-via binder is REJECTED (fail-closed --
       pinned here), whereas the named form's [refine_] works
       (lean_via.ml [unpack_idiom]).
   (b) reaching the skeleton of an inline-via value ([let r : (...
       [@vox.via ...]) = ... in (r : tree)]) is silently ALLOWED -- an
       implicit projection the named form correctly rejects
       (lean_via.ml [no_projection]).  This is fail-OPEN relative to
       the named form, so per policy it is NOT pinned as blessed
       behaviour; it is lossy (drops the link fact) but not a soundness
       hole -- result-position obligations and the recovered binder
       fact are unaffected.

   The fix is structural (the value-side handling of a literal
   [Trefine] via, upstream of these guards), not a filtered-attribute
   one-liner, so it is reported rather than forced. *)
let refine_inline_rejected
  : (t : (tree{ bst _ } [@vox.via (elems : iset)])) -> unit =
  fun t ->
    let refine_ t0 = t in
    let _ : unit{ bst t0 } = () in
    ()
[%%expect{|
Line 4, characters 8-18:
4 |     let refine_ t0 = t in
            ^^^^^^^^^^
Error: vox: a refine_ pattern requires the scrutinee to have a refined type (a plain let binds at the skeleton and carries the fact already)
|}]
