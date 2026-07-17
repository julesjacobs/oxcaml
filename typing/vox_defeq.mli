(* Definitional equations for total functions.  See
   [Vox_defeq] implementation and design/definitional-equations-synthesis.md. *)

(* Expand every [let[@vox.def] f p1 ... pn = rhs] structure item into the
   original binding for [f] (forced [@ total]) followed by a companion
   trusted-lemma binding [f_def].  Structures with no such binding are returned
   unchanged. *)
val expand_structure : Parsetree.structure -> Parsetree.structure

(* Marker attribute placed on the generated companion lemma binding, so that
   [Vox_verify] treats its refinement as a trusted axiom and does not re-verify
   its body. *)
val axiom_attribute : string
