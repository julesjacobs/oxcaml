let stable = 1
let stable_value : int{ _ = stable } = stable
let consume (value : int{ _ = stable }) = value

let local_scope () =
  let local = 1 in
  let alias = local in
  let nested_alias = alias in
  let value : int{ _ = nested_alias } = nested_alias in
  ignore value

let shadowing () =
  let stable = 2 in
  let first : int{ _ = stable } = stable in
  let stable = 3 in
  let second : int{ _ = stable } = stable in
  ignore (first, second)

let dependent (value : int) : int{ _ = value } = value

module Inner = struct
  let stable = 2
  let stable_value : int{ _ = stable } = stable
  let consume (value : int{ _ = stable }) = value
end

module Make () = struct
  let stable = 3
  let stable_value : int{ _ = stable } = stable
  let consume (value : int{ _ = stable }) = value
end

type positive = { field : int{ _ > 0 } }
let ordinary value = { field = value }
let cases = function value -> { field = value }
