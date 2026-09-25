let bad : (a : Reference_lock.t) ->
    {t : int Ghost_pref.token | Reference_lock.owned a (Ghost_pref.own t)}
      @ local read ghost -> {n : int | n < 0} =
  fun a t -> Reference_lock.read_owned a t
