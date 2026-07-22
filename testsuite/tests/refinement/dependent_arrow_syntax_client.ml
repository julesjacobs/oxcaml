let seven : int{ _ = 7 } = Dependent_arrow_syntax_api.identity 7

let eleven : int{ _ = 11 } =
  Dependent_arrow_syntax_api.labeled_identity ~value:11
