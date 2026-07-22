let reject ~(left : Parameter_import_provider.t @ logical)
    ~(right : Parameter_import_provider.t @ logical)
    ~(other_left : Parameter_import_provider.t @ logical)
    ~(other_right : Parameter_import_provider.t @ logical)
    ~(witness : unit{
        Parameter_import_provider.equal other_left other_right = true
      })
    ~(query : int) =
  Parameter_import_provider.consume ~left ~right ~witness ~query
