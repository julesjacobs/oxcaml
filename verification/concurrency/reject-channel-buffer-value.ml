let bad (r : Channel_buffer.filled @ unique) :
    {n : int | n <> r.expected} =
  let n, _ = Channel_buffer.read_receipt r in n
