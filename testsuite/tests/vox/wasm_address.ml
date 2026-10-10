module B = Wasm_u32

type memory_size = {n : int | 0 <= n && n <= 4294967296}
type access_width = {n : int | 1 <= n && n <= 8}

let[@def] (in_bounds @ total) (size : memory_size) (base : B.u32)
    (offset : B.u32) (width : access_width) =
  base + offset + width <= size

let (resolve @ total) : (size : memory_size) -> (base : B.u32) ->
    (offset : B.u32) -> (width : access_width) ->
    {result : B.u32 option | match result with
      | None -> not (in_bounds size base offset width)
      | Some address -> in_bounds size base offset width
        && address = base + offset && address + width <= size} =
  fun size base offset width ->
    ghost_ (in_bounds_def size base offset width);
    let address = base + offset in
    if address + width <= size then Some address else None
