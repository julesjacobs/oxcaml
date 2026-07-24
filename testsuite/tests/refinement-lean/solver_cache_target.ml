let clamp (n : int) =
  (if n > 0 then n else 100 : int{ _ >= 0 })

