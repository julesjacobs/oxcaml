open struct
  let hidden = 13
end

let leaked : int{ _ = hidden } = hidden
