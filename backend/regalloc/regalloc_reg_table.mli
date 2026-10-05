type 'a t

val create : int -> 'a t

val clear : 'a t -> unit

val find : 'a t -> Reg.t -> 'a

val replace : 'a t -> Reg.t -> 'a -> unit

val fold : (Reg.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
