let law ~(key : int @ logical)
    : unit{ Lpi_collision_other.key = 0 }
  =
  let _other = Lpi_collision_other.key_is_zero () in
  ()
