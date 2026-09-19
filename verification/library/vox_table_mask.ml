module M = Vox_table_model

type lane = {i : int | 0 <= i && i < 16}
type count = {n : int | 0 <= n && n <= 16}

let[@def] (prefix @ total) (count : int) =
  if count <= 0 then 0 else
    if count >= 16 then 65535 else M.lane_bit count - 1

let (lane_bounds @ total) (lane : lane) :
    {u : unit | 0 < M.lane_bit lane && M.lane_bit lane <= 32768} =
  M.lane_bit_def lane;
  ()

let (prefix_step @ total) (lane : lane) :
    {u : unit | prefix (lane + 1) = prefix lane lor M.lane_bit lane &&
      prefix lane land M.lane_bit lane = 0 &&
      0 <= prefix lane && prefix lane <= 65535 &&
      0 <= prefix (lane + 1) && prefix (lane + 1) <= 65535} =
  prefix_def lane; prefix_def (lane + 1);
  M.lane_bit_def lane; M.lane_bit_def (lane + 1);
  ()

let (distinct_lanes @ total) (left : lane) (right : lane) :
    {u : unit | left = right ||
      M.lane_bit left land M.lane_bit right = 0} =
  M.lane_bit_def left; M.lane_bit_def right;
  ()

let rec (bounded @ total) : ('k : immutable_data) ('v : immutable_data).
    (model : ('k, 'v) M.state) @ immutable ->
    (offset : int) -> (needle : int) -> (count : count) ->
    {u : unit | M.matching model offset needle count land
      (prefix count lxor (-1)) = 0 &&
      0 <= M.matching model offset needle count &&
      M.matching model offset needle count <= 65535} @ ghost =
  fun model offset needle count -> ghost_ (
    M.matching_def model offset needle count;
    if count > 0 then begin
      bounded model offset needle (count - 1);
      prefix_step (count - 1);
      lane_bounds (count - 1);
      ()
    end else begin prefix_def count; () end)
  [@@decreases count]

let rec (get @ total) : ('k : immutable_data) ('v : immutable_data).
    (model : ('k, 'v) M.state) @ immutable ->
    (offset : int) -> (needle : int) -> (count : count) ->
    (lane : {i : int | 0 <= i && i < count}) ->
    {u : unit | (M.matching model offset needle count land M.lane_bit lane
      <> 0) = (M.control model (offset + lane) === Some needle)} @ ghost =
  fun model offset needle count lane -> ghost_ (
    M.matching_def model offset needle count;
    bounded model offset needle (count - 1);
    prefix_step (count - 1);
    lane_bounds lane;
    if lane = count - 1 then () else begin
      distinct_lanes lane (count - 1);
      get model offset needle (count - 1) lane;
      ()
    end)
  [@@decreases count]
