let false_from_name_collision =
  let () = Lpi_collision_provider.law ~key:1 in
  (1 : int{ _ = 0 })
