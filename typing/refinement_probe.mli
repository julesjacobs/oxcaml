(* Refinement probe (-drefinements), from design-docs/refinement-flow.md.

   A flag-gated walk of a typed structure that prints:
   - every expression node whose [exp_type] has a refined head (after alias
     expansion), with its location — under the refinement-flow invariant such
     a head can only be the residue of a type variable later solved against a
     declared refined type, so on most programs this report is empty and a
     disabled strip adds lines to it;
   - every pattern-bound variable whose environment entry has a refined head,
     tagged distinctly — this is what makes the binder strip observable:
     local immutable binders never appear, while the exempted entries
     (mutable and module-level binders) do;
   - every recorded [Texp_refinement_obligation] marker, tagged
     "refinement obligation", one line per marker — the complete obligation
     map, and a duplicated record at a site shows up as a duplicated line.

   An observer, not an assertion: the variable-solving residue is legal, so
   the expected output is the judge. *)

val implementation : Format.formatter -> Typedtree.structure -> unit
