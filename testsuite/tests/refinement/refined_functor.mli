module type Input = sig
  val witness : int
end

module Make :
  functor (X : Input) -> sig
    val result : int{ _ = X.witness }
  end
