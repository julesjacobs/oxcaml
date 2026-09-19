module M = Vox_table_model

type nonempty = {m : int | 0 < m && m <= 65535}

let (first @ total) (mask : nonempty) :
    {i : int | 0 <= i && i < 16 &&
      (mask land M.lane_bit i) <> 0 &&
      (mask land (M.lane_bit i - 1)) = 0} =
  if mask land 255 = 0 then
    if mask land 3840 = 0 then
      if mask land 12288 = 0 then
        if mask land 16384 = 0 then
          (ghost_ (M.lane_bit_def 15); refine_ 15)
        else
          (ghost_ (M.lane_bit_def 14); refine_ 14)
      else
        if mask land 4096 = 0 then
          (ghost_ (M.lane_bit_def 13); refine_ 13)
        else
          (ghost_ (M.lane_bit_def 12); refine_ 12)
    else
      if mask land 768 = 0 then
        if mask land 1024 = 0 then
          (ghost_ (M.lane_bit_def 11); refine_ 11)
        else
          (ghost_ (M.lane_bit_def 10); refine_ 10)
      else
        if mask land 256 = 0 then
          (ghost_ (M.lane_bit_def 9); refine_ 9)
        else
          (ghost_ (M.lane_bit_def 8); refine_ 8)
  else
    if mask land 15 = 0 then
      if mask land 48 = 0 then
        if mask land 64 = 0 then
          (ghost_ (M.lane_bit_def 7); refine_ 7)
        else
          (ghost_ (M.lane_bit_def 6); refine_ 6)
      else
        if mask land 16 = 0 then
          (ghost_ (M.lane_bit_def 5); refine_ 5)
        else
          (ghost_ (M.lane_bit_def 4); refine_ 4)
    else
      if mask land 3 = 0 then
        if mask land 4 = 0 then
          (ghost_ (M.lane_bit_def 3); refine_ 3)
        else
          (ghost_ (M.lane_bit_def 2); refine_ 2)
      else
        if mask land 1 = 0 then
          (ghost_ (M.lane_bit_def 1); refine_ 1)
        else
          (ghost_ (M.lane_bit_def 0); refine_ 0)

type mask = {m : int | 0 <= m && m <= 65535}

let[@def] (clear @ total) (mask : int) :
    {r : int | r = mask land (mask - 1) &&
      (not (0 <= mask && mask <= 65535) ||
        0 <= r && r <= 65535 && (mask = 0 || r < mask))} =
  mask land (mask - 1)

let (clear_lane @ total) (mask : nonempty)
    (lane : {i : int | 0 <= i && i < 16}) :
    {u : unit | (clear mask land M.lane_bit lane <> 0) =
      (mask land M.lane_bit lane <> 0 && lane <> first mask)} =
  let chosen = first mask in
  clear_def mask;
  M.lane_bit_def lane; M.lane_bit_def chosen;
  ()
