@@ portable

module type S = sig
  type token : void

  type partition = { left : token @@ ghost; right : token @@ ghost }

  val credits : token @ local immutable total ghost -> int @ ghost @@ total

  val nonnegative : (token : token) @ local immutable total ghost ->
    {u : unit | 0 <= credits token} @ ghost @@ total

  val empty : unit -> {t : token | credits t = 0} @ unique total ghost @@ total

  val tick : (token : {t : token | credits t > 0})
      @ unique total ghost ->
    {t : token | credits t = credits token - 1} @ unique total ghost @@ total

  val split : (amount : int) @ ghost ->
    (token : {t : token | 0 <= amount && amount <= credits t})
      @ unique total ghost ->
    {p : partition | credits p.left = amount &&
      credits p.right = credits token - amount} @ unique @@ total

  val merge : (left : token) @ unique total ghost ->
    (right : {t : token | 0 <= credits left && 0 <= credits t &&
      0 <= credits left + credits t})
      @ unique total ghost ->
    {t : token | credits t = credits left + credits right} @ unique total ghost @@ total

end

module Make () : sig
  include S
  module Budget : sig
  val create : (amount : {n : int | n >= 0}) @ ghost ->
    {t : token | credits t = amount}
      @ unique total ghost @@ total
end
end
