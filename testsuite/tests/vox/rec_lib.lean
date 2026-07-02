-- Spec functions for lean_records.ml: a measure over a record
-- (structures expose projections directly).
@[grind] def norm1 (p : Vox_Lean_records_point) : Int := p.px + p.py
