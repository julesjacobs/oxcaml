type node = { value : int; next : node Handle.t }
let (value @ total) node = node.value

module type Phantom = sig type 'a t : value mod logical end
module H : Phantom = Handle
type aliased = { next : aliased H.t }
let (next @ total) node = node.next
