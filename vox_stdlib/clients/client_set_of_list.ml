(* Wave-3 cross-module client + R7 ACCEPTANCE GATE (integrator-owned):
   Vlist + Vset in ONE goal that forces BOTH modules' algebra to fire
   cross-unit. Verifies against Vlist and Vset artifacts (cmi + VoxSig olean);
   the Vset half needs wave-2's artifacts, so this is staged ready-to-run and
   verified at part 3 once Vset exists.

   head_in_both: cons x onto a list and add x to a set, then observe x is a
   member of BOTH. `inl` = ll_mem x (ll_cons x l) is true by Vlist's
   ll_mem_cons; `ins` = vs_mem x (result of Vset.add x s) is true by Vset's
   vs_addspec (its ∀-law instantiated at y = x). The `&&` (fact-threading
   confirmed live in this clone) makes the goal `_ = true` require BOTH laws:
   remove ll_mem_cons and inl is not provably true; remove vs_addspec and ins
   is not. Models LList and ISet co-exist in the client with no name clash
   (the per-unit ll_ and vs_ prefixes), which is the F2-safe R7 composition.

   NOTE (integrator): written against the blueprint §3 Vset surface
   (ops empty/add/mem; model vs_mem / vs_addspec). If build-vset shipped
   different op/law names, adjust these three references at part-3 verify. *)

let head_in_both (x : int) (l : Vlist.t) (s : Vset.t) : bool{ _ = true } =
  (* Vlist.cons is equational -> inlines into mem (C1 REMOVED); Vset.add is
     RELATIONAL (vs_addspec) -> s' KEPT; inl/ins bound for the && threading. *)
  let s' = Vset.add x s in
  let inl = Vlist.mem x (Vlist.cons x l) in
  let ins = Vset.mem x s' in
  inl && ins
