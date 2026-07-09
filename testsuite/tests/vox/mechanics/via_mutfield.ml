(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* MUTABLE via-typed record field (STAGE 4 sharp case).  A via type is a
   fine field type, but vox bars ANY mutable field from refinement
   predicates (mutable state is framed by the flow analysis, never named
   in a fact), so the "which sort does its version speak at" question
   never arises for a mutable via field -- it is unreachable by
   construction.  The restriction is GENERAL, not via-specific (the same
   error fires for a mutable [int] field), so via adds nothing here.  The
   reachable case -- an IMMUTABLE via field -- is nameable and speaks at
   the IMAGE sort, consistent with image-binder. *)

type iset [@@vox.sort lean "ISet"]
type tree = Leaf | Node of tree * int * tree
type t = tree{ bst _ } [@vox.via (elems : iset)]

type mbox = { mutable contents : t }

(* A mutable via field cannot appear in a refinement -- unreachable. *)
let get_mut (b : mbox) : t{ _ = b.contents } = b.contents
[%%expect{|
type iset
type tree = Leaf | Node of tree * int * tree
type t = tree{ bst _ via (elems : iset) }
type mbox = { mutable contents : t; }
Line 8, characters 32-42:
8 | let get_mut (b : mbox) : t{ _ = b.contents } = b.contents
                                    ^^^^^^^^^^
Error: vox: only fields of simple records (monomorphic, no mutable fields) may appear in refinement predicates
|}]

(* The SAME error fires for a mutable non-via field: the restriction is
   general, not a via limitation. *)
type ibox = { mutable n : int }
let get_int (b : ibox) : int{ _ = b.n } = b.n
[%%expect{|
type ibox = { mutable n : int; }
Line 2, characters 34-37:
2 | let get_int (b : ibox) : int{ _ = b.n } = b.n
                                      ^^^
Error: vox: only fields of simple records (monomorphic, no mutable fields) may appear in refinement predicates
|}]
