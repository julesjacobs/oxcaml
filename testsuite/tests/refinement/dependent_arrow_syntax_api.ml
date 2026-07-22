let identity (x : int) : int{ _ = x } = x

let labeled_identity ~value : int{ _ = value } = value

let unused_name value = value
