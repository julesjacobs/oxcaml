val stable : int
val stable_value : int{ _ = stable }
val consume : int{ _ = stable } -> int
val dependent : (value : int) -> int{ _ = value }

module Inner : sig
  val stable : int
  val stable_value : int{ _ = stable }
  val consume : int{ _ = stable } -> int
end

module Make () : sig
  val stable : int
  val stable_value : int{ _ = stable }
  val consume : int{ _ = stable } -> int
end
