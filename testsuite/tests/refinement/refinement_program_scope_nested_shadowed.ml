module Inner = struct
  let anchor = 1
  let old : int{ _ = anchor } = anchor
  let anchor = 2
end
