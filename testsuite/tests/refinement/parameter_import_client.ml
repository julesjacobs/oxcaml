let apply ~(left : Parameter_import_provider.t @ logical)
    ~(right : Parameter_import_provider.t @ logical)
    ~(witness : unit{
        Parameter_import_provider.equal left right = true
      })
    ~(query : int) =
  Parameter_import_provider.consume ~left ~right ~witness ~query
