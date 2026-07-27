module Make (K : Key_intf.ORDERED_KEY) : sig
  include Key_intf.SET with type key = K.t

  (* Not part of [SET]; see the note in [key_intf.ml]. *)
  val shape : t -> key list
end
