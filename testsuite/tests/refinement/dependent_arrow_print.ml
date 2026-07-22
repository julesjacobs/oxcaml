let identity (x : int) : int{ _ = x } = x

let labeled_identity ~value : int{ _ = value } = value

let nested_shadow (x : int) (x : int{ _ >= x }) : int{ _ = x } = x

let unused_name value = value
