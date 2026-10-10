let rejected x =
  let present = Provider.Alias.mem x (Provider.Alias.Refined.singleton x) in
  let proof : {b : bool | b} = refine_ present in
  let refine_ proof = proof in
  ()
