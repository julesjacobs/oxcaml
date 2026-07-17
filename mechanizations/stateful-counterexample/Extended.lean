import Pifo
open Pifo

-- Extended bounded check via native_decide (compiled, fast) as supporting
-- evidence for the all-n flush-agreement claim (NOT a proof for all n).
theorem flush_agree_le_30 :
    ∀ n, n ≤ 30 → drainS1 n = drainS2 n ∧ drainS1 n = expectedDrain n := by
  native_decide
