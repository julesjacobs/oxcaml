-- Spec library for lean_binsearch.ml: an immutable integer array is
-- modelled by two uninterpreted functions, [len] for its length and
-- [elem] for its contents.  Deliberately NO axioms: every fact about
-- [len] and [elem] enters through the two assume_unchecked_ wrappers
-- in the OCaml source, so the verification is valid for every array.
opaque len : VoxU -> Int
opaque elem : VoxU -> Int -> Int
