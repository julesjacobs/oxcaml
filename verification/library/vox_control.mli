type mask = {m : int | 0 <= m && m <= 65535}

(** Trusted exact byte matching: bit [i] is set iff byte [offset+i] equals
    [byte]. Loads exactly 16 bytes. Raises [Invalid_argument] for invalid
    bounds or a byte outside 0..255. Uses NEON, SSE2, or a scalar fallback. *)
val match16 : bytes -> int -> int -> mask

val fingerprint : int -> {b : int | 0 <= b && b <= 127}
val clear_first : (m : mask) ->
  {r : mask | r = (m land (m - 1)) && (m = 0 || r < m)}
