(* Definitional equations for total functions.  See
   [Vox_defeq] implementation and design/definitional-equations-synthesis.md. *)

(* Expand every [let[@vox.def] [rec] f p1 ... pn = rhs] structure item into the
   original binding for [f] (forced [@ total], preserving recursion) followed
   by a companion trusted-lemma binding [f_def].  Structures with no such
   binding are returned unchanged. *)
val expand_structure : Parsetree.structure -> Parsetree.structure

(* True for a binding produced by [expand_structure] as a companion lemma:
   recognised by the physical identity of the fresh ghost location the expander
   minted for it, an out-of-band channel that user surface syntax cannot forge.
   [Vox_verify] consults this to treat such a lemma's refinement as a trusted
   axiom (skipping verification of its unit body); every other binding, however
   attributed, is verified normally. *)
val is_generated_lemma_loc : Location.t -> bool
