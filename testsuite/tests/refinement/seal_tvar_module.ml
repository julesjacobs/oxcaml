module M : sig
  val x : int{ _ > 0 }
end = struct
  let x = failwith "u"
end
