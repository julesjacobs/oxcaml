type positive = { field : int{ _ > 0 } }
let annotated = function (value : int) -> { field = value }
