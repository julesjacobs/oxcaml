let fact_after_saturation =
  let partial = Lpi_order_api.law ~x:11 in
  let () = partial ~y:22 in
  (Lpi_order_api.relation 11 22 : bool{ _ = true })
