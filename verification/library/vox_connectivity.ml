module Make (C : Vox_big_credits.S) = struct
  include Vox_union_find_online.Make (C)
  (* The wrappers share the observation bindings so their dependent
     refinement contracts survive signature sealing. *)
  let create = create_connectivity
  let make_set = make_set_connectivity
  let find = find_connectivity
  let union = union_connectivity
end
