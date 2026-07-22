module Inner : sig
  val anchor : int
  val old : int{ _ = anchor }
end
