module F (M : sig val value : int end) = struct
  let field = M.value
end

module X = struct let value = 1 end
module Y = struct let value = 2 end
module A = F (X)
module B = F (Y)
