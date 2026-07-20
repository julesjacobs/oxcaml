let no_fact_before_saturation =
  let _partial = Lpi_order_api.law ~x:11 in
  (Lpi_order_api.relation 11 22 : bool{ _ = true })
