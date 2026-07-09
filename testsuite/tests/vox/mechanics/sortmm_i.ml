type t = { mutable c : int }

let mk : (v : int) -> t{ _ = v } = fun v -> assume_unchecked_ { c = v }
