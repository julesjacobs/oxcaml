module M = Vox_table_model

external count_trailing_zeros : int -> int @@ total
  = "caml_vox_int_ctz" "caml_vox_int_ctz_untagged"
  [@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

type nonempty = {m : int | 0 < m && m <= 65535}

let[@inline always] (first @ total) (mask : nonempty) :
    {i : int | 0 <= i && i < 16 &&
      (mask land M.lane_bit i) <> 0 &&
      (mask land (M.lane_bit i - 1)) = 0} =
  let i = count_trailing_zeros mask in
  ghost_ (M.lane_bit_def i);
  i

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
