module Included = struct
  include Specification_only_provider
end

module Via_type : module type of Specification_only_provider =
  Specification_only_provider

module Preserve (X : module type of Specification_only_provider) = struct
  module Inner = X
end

module Through_functor = Preserve (Specification_only_provider)

let include_refinement (x : int)
    : unit{ Included.project x = Included.project x }
  = ()

let module_type_refinement (x : int)
    : unit{ Via_type.project x = Via_type.project x }
  = ()

let functor_refinement (x : int)
    : unit{
        Through_functor.Inner.project x
        = Through_functor.Inner.project x
      }
  = ()
