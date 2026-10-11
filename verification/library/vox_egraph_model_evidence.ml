(* The implementation of [Vox_egraph_rule_handle.same_class]: compares the
   union-find roots of two ids and, when they agree, returns the erased
   derivation from [Vox_egraph_match_evidence.same]. The ghost block relates
   the answer to the model: [Q.same] on the model is equality of roots
   ([Vox_egraph_match_observation.same]), and the model origin of each id is
   its ghost origin ([Vox_egraph_snapshot_proof.origin]). *)

module V = Vox_egraph_rule_store
module P = Vox_egraph_match_evidence
module O = Vox_egraph_match_observation
module Q = Vox_egraph_match_spec
module E = Vox_egraph_derivation_spec
module M = Vox_egraph_union_spec
module S = Vox_egraph_snapshot_spec
module SP = Vox_egraph_snapshot_proof

type result = #{equal : bool; proof : E.evidence option @@ ghost}

let (query @ total) : (state : {s : V.t | V.valid s}) @ immutable ->
    (a : {i : int | 0 <= i && i < state.semantic.union.count}) ->
    (b : {i : int | 0 <= i && i < state.semantic.union.count}) ->
    {r : result | r.#equal = Q.same (P.view state) a b &&
      (a <> b || r.#equal) &&
      (match r.#proof with
       | None -> not r.#equal
       | Some proof -> r.#equal && E.valid state.semantic.rules proof &&
         S.origin (P.view state) a === Some (E.left proof) &&
         S.origin (P.view state) b === Some (E.right proof))} =
  fun state a b ->
    ghost_ (
      P.bounds state ();
      P.view_def state;
      O.same state.nodes state.semantic.union.parents state.semantic.union.count a b ();
      SP.origin state a ();
      SP.origin state b ());
    let equal = M.root state.semantic.union.parents a = M.root state.semantic.union.parents b in
    let proof = ghost_ (if equal then Some (P.same state a b ()) else None) in
    #{equal; proof}
