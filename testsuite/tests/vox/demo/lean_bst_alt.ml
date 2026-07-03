(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "bst.mli bst.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An ALTERNATIVE, quantified interface built OVER Bst -- the module
   itself is untouched.  Bst exports the characterization of insertion
   as a THEOREM (mem_insert, through the .cmi); with quantifiers in
   predicates the same statement can live in a TYPE, so the set-like
   signature below says, in one refinement, everything [add] does:
   the result holds exactly the old elements plus [x].  Proving it is
   the imported lemma meeting a [forall_] goal; consuming it is
   instantiation at whatever key a client asks about. *)

open Bst

module Set : sig
  val add
    : (x : int) -> (t : set)
      -> tree{ bst _ && (forall_ y. mem y _ = (y = x || mem y t)) }
end = struct
  let add
    : (x : int) -> (t : set)
      -> tree{ bst _ && (forall_ y. mem y _ = (y = x || mem y t)) }
    =
    fun x t ->
      let t' = insert x t in
      t'
end

(* Consuming the quantified fact: adding 2 does not affect membership
   of 7 -- the forall_ instantiates at the key the client asks about,
   and [member]'s answer about the NEW tree is proved to answer the
   question about the OLD one. *)
let probe : (t : set) -> bool{ _ = mem 7 t } =
  fun t ->
    let t' = Set.add 2 t in
    let b = member 7 t' in
    b

(* An exists_ goal by the witness-equation idiom: the added key
   witnesses nonemptiness. *)
let singleton : (x : int) -> tree{ bst _ && (exists_ y. y = x && mem y _) } =
  fun x ->
    let e = empty in
    let t = Set.add x e in
    t
