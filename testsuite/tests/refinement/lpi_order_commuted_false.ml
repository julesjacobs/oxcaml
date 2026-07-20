let argument_order_is_preserved =
  let () = Lpi_order_api.law ~x:11 ~y:22 in
  (Lpi_order_api.relation 22 11 : bool{ _ = true })
