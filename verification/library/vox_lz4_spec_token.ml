let (match_token @ total) :
    (literals : {n : int | 0 <= n && n <= 4194304}) ->
    (match_code : {n : int | 0 <= n && n <= 4194304}) ->
    {token : Vox_lz4_spec_parse.byte | token =
      16 * (if literals >= 15 then 15 else literals)
      + (if match_code >= 15 then 15 else match_code)} =
  fun literals match_code ->
    let high = if literals >= 15 then 15 else literals in
    let low = if match_code >= 15 then 15 else match_code in
    (16 * high + low)

type distance_bytes = { low : Vox_lz4_spec_parse.byte; high : Vox_lz4_spec_parse.byte }

let (split_distance @ total) :
    (distance : {d : int | 0 <= d && d <= 65535}) ->
    {r : distance_bytes | distance = r.low + 256 * r.high} =
  fun distance ->
    let low = distance in
    let high = 0 in
    let low, high =
      if low >= 32768 then low - 32768, high + 128
      else low, high in
    let low, high =
      if low >= 16384 then low - 16384, high + 64
      else low, high in
    let low, high =
      if low >= 8192 then low - 8192, high + 32
      else low, high in
    let low, high =
      if low >= 4096 then low - 4096, high + 16
      else low, high in
    let low, high =
      if low >= 2048 then low - 2048, high + 8
      else low, high in
    let low, high =
      if low >= 1024 then low - 1024, high + 4
      else low, high in
    let low, high =
      if low >= 512 then low - 512, high + 2
      else low, high in
    let low, high =
      if low >= 256 then low - 256, high + 1
      else low, high in
    { low = low; high = high }
