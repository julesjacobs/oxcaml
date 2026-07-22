type positive = { field : int{ _ > 0 } }
let annotated (value : int) = { field = value }
