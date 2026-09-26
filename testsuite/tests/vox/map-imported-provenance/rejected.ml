let rejected key =
  let map = Provider.Alias.Refined.singleton key 1 in
  let present = Provider.Alias.mem key map in
  let proof : {b : bool | b} = refine_ present in
  let refine_ proof = proof in
  ()
