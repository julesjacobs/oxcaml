type t = { mutable c : int } [@@vox.sort lean "IBag"]

let mk : (x : int) -> t = fun x -> { c = x }
