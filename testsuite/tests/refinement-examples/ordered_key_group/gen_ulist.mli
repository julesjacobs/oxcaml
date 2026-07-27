module Make (K : Key_intf.ORDERED_KEY) :
  Key_intf.SET with type key = K.t
