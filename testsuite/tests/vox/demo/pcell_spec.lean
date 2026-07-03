-- Token axioms for pcell_lib: cells and tokens live at the
-- uninterpreted sort; cid/tid/cts are opaque (the API's refinements
-- are the only source of facts about them).  The allocation pair is a
-- simple record whose projections vox models natively, so no datatype
-- is referenced here.
opaque cid : VoxU -> Int
opaque tid : VoxU -> Int
opaque cts : VoxU -> Int
