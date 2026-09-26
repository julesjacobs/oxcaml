module Make (Element : Polymorphic_set_intf.Ordered) :
  Polymorphic_set_intf.S with module Element = Element
