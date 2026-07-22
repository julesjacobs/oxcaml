let apply ~(left : Parameter_import_domain.t @ logical)
    ~(right : Parameter_import_domain.t @ logical)
    ~(witness : unit{ Parameter_import_domain.equal left right = true }) =
  Parameter_import_domain.consume ~left ~right ~witness
