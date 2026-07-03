-- Spec library for lean_kernel.ml: an immutable integer array is
-- modelled by an uninterpreted length; contents never enter the
-- logic (the kernel proves in-bounds, nothing else).
opaque len : VoxU -> Int
