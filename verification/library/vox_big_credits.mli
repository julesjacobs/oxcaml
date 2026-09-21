@@ portable

module type S = sig
  type token : void

  type partition = { left : token @@ ghost; right : token @@ ghost }

  val credits : token @ local immutable total ghost -> Bigint.t @ ghost @@ total

  val nonnegative : (token : token) @ local immutable total ghost ->
    {u : unit | 0Z <= credits token} @ ghost @@ total

  val empty : unit -> {t : token | credits t = 0Z} @ unique total ghost @@ total

  val tick : (token : {t : token | credits t > 0Z})
      @ unique total ghost ->
    {t : token | let refine_ token = token in
      credits t = Bigint.sub (credits token) 1Z} @ unique total ghost @@ total

  val split : (amount : Bigint.t) @ ghost ->
    (token : {t : token | 0Z <= amount && amount <= credits t})
      @ unique total ghost ->
    {p : partition | let refine_ token = token in credits p.left = amount &&
      credits p.right = Bigint.sub (credits token) amount} @ unique @@ total

  val merge : (left : token) @ unique total ghost ->
    (right : {t : token | 0Z <= credits left && 0Z <= credits t})
      @ unique total ghost ->
    {t : token | let refine_ right = right in
      credits t = Bigint.add (credits left) (credits right)} @ unique total ghost @@ total

end

module Make () : sig
  include S
  module Budget : sig
  val create : (amount : {n : Bigint.t | n >= 0Z}) @ ghost ->
    {t : token | let refine_ amount = amount in credits t = amount}
      @ unique total ghost @@ total
end
end
