let[@def] (addmod @ total) (modulus : int) (value : int) (delta : int) =
  if value < modulus - delta then value + delta
  else value - (modulus - delta)

let[@def] (low @ total) (half : int) (value : int) =
  if value < half then value else value - half

let[@def] (flip @ total) (half : int) (value : int) =
  if value < half then value + half else value - half

type half = {n : int | 0 < n && n <= 67108864}

let (addmod_range @ total)
    (modulus : {m : int | 0 < m && m <= 134217728})
    (value : {v : int | 0 <= v && v < modulus})
    (delta : {d : int | 0 <= d && d <= modulus}) :
    {u : unit | 0 <= addmod modulus value delta &&
      addmod modulus value delta < modulus} =
  addmod_def modulus value delta;
  ()

let (low_range @ total) (half : half)
    (value : {v : int | 0 <= v && v < half + half}) :
    {u : unit | 0 <= low half value && low half value < half} =
  low_def half value;
  ()

let (flip_laws @ total) (half : half)
    (value : {v : int | 0 <= v && v < half + half}) :
    {u : unit | 0 <= flip half value && flip half value < half + half &&
      flip half (flip half value) = value && flip half value <> value &&
      low half (flip half value) = low half value} =
  flip_def half value;
  flip_def half (flip half value);
  low_def half value;
  low_def half (flip half value);
  ()

let (low_pair @ total) (half : half)
    (left : {v : int | 0 <= v && v < half + half})
    (right : {v : int | 0 <= v && v < half + half}) :
    {u : unit | not (low half left = low half right) ||
      left = right || flip half left = right} =
  low_def half left;
  low_def half right;
  flip_def half left;
  ()

let (low_addmod @ total) (half : half)
    (value : {v : int | 0 <= v && v < half + half})
    (delta : {d : int | 0 <= d && d <= half}) :
    {u : unit | low half (addmod (half + half) value delta) =
      addmod half (low half value) delta} =
  addmod_def (half + half) value delta;
  low_def half value;
  low_def half (addmod (half + half) value delta);
  addmod_def half (low half value) delta;
  ()

let (reverse_step @ total) (half : half)
    (value : {v : int | 0 <= v && v < half + half})
    (delta : {d : int | 0 < d && d < half}) :
    {u : unit | addmod (half + half)
      (flip half (addmod (half + half) value delta))
      (half + half - delta) = flip half value} =
  addmod_def (half + half) value delta;
  flip_def half (addmod (half + half) value delta);
  addmod_def (half + half)
    (flip half (addmod (half + half) value delta))
    (half + half - delta);
  flip_def half value;
  ()

let (seam @ total) (half : half)
    (value : {v : int | 0 <= v && v < half + half}) :
    {u : unit | addmod (half + half) value half = flip half value} =
  addmod_def (half + half) value half;
  flip_def half value;
  ()

let[@def] rec (triangle @ total) (modulus : int) (rank : int) =
  if rank <= 0 then 0 else
    addmod modulus (triangle modulus (rank - 1)) rank
  [@@decreases if rank > 0 then rank else 0]

let rec (triangle_range @ total) :
    (modulus : {m : int | 0 < m && m <= 134217728}) ->
    (rank : {r : int | 0 <= r && r < modulus}) ->
    {u : unit | 0 <= triangle modulus rank &&
      triangle modulus rank < modulus} = fun modulus rank ->
  triangle_def modulus rank;
  if rank > 0 then begin
    triangle_range modulus (rank - 1);
    addmod_range modulus (triangle modulus (rank - 1)) rank;
    ()
  end else ()
  [@@decreases rank]

let rec (low_triangle @ total) : (half : half) ->
    (rank : {r : int | 0 <= r && r < half}) ->
    {u : unit | low half (triangle (half + half) rank) =
      triangle half rank} = fun half rank ->
  triangle_def (half + half) rank;
  triangle_def half rank;
  low_def half (triangle (half + half) rank);
  if rank > 0 then begin
    low_triangle half (rank - 1);
    triangle_range (half + half) (rank - 1);
    low_addmod half (triangle (half + half) (rank - 1)) rank;
    ()
  end else ()
  [@@decreases rank]

let rec (reflection @ total) : (half : half) ->
    (rank : {r : int | 0 <= r && r < half}) ->
    {u : unit | triangle (half + half) (half + half - 1 - rank) =
      flip half (triangle (half + half) rank)} = fun half rank ->
  triangle_def (half + half) (half + half - 1 - rank);
  triangle_range (half + half) rank;
  if rank = half - 1 then begin
    seam half (triangle (half + half) rank);
    ()
  end else begin
    reflection half (rank + 1);
    triangle_def (half + half) (rank + 1);
    reverse_step half (triangle (half + half) rank) (rank + 1);
    ()
  end
  [@@decreases half - rank]

type plan = One | Twice of plan [@@inductive]

let[@def] rec (groups @ total) (plan : plan @ immutable) = match plan with
  | One -> 1
  | Twice previous -> let n = groups previous in n + n

let[@def] rec (valid @ total) (plan : plan @ immutable) = match plan with
  | One -> true
  | Twice previous -> valid previous && groups previous <= 67108864

let rec (groups_range @ total) : (plan : plan) @ immutable ->
    {u : unit | not (valid plan) ||
      0 < groups plan && groups plan <= 134217728} = fun plan ->
  valid_def plan; groups_def plan;
  match plan with
  | One -> ()
  | Twice previous -> groups_range previous; ()

let[@def] rec (inverse @ total) (plan : plan @ immutable) (offset : int) =
  match plan with
  | One -> 0
  | Twice previous ->
    let half = groups previous in
    let rank = inverse previous (low half offset) in
    if triangle (half + half) rank = offset then rank
    else half + half - 1 - rank

let rec (inverse_at @ total) :
    (plan : plan) @ immutable -> (offset : int) ->
    {u : unit | not (valid plan && 0 <= offset && offset < groups plan) ||
      0 <= inverse plan offset &&
      inverse plan offset < groups plan &&
      triangle (groups plan) (inverse plan offset) = offset} =
  fun plan offset ->
  if valid plan && 0 <= offset && offset < groups plan then begin
  valid_def plan; groups_def plan; groups_range plan;
  inverse_def plan offset;
  match plan with
  | One -> triangle_def 1 0; ()
  | Twice previous ->
    groups_range previous;
    let half = groups previous in
    low_range half offset;
    inverse_at previous (low half offset);
    let rank = inverse previous (low half offset) in
    triangle_range (half + half) rank;
    low_triangle half rank;
    low_pair half (triangle (half + half) rank) offset;
    reflection half rank;
    ()
  end else ()

let rec (inverse_rank @ total) :
    (plan : plan) @ immutable -> (rank : int) ->
    {u : unit | not (valid plan && 0 <= rank && rank < groups plan) ||
      inverse plan (triangle (groups plan) rank) = rank} =
  fun plan rank ->
  if valid plan && 0 <= rank && rank < groups plan then begin
  valid_def plan; groups_def plan; groups_range plan;
  inverse_def plan (triangle (groups plan) rank);
  match plan with
  | One -> ()
  | Twice previous ->
    groups_range previous;
    let half = groups previous in
    if rank < half then begin
      low_triangle half rank;
      inverse_rank previous rank;
      ()
    end else begin
      let reflected = half + half - 1 - rank in
      reflection half reflected;
      triangle_range (half + half) reflected;
      flip_laws half (triangle (half + half) reflected);
      low_triangle half reflected;
      inverse_rank previous reflected;
      ()
    end
  end else ()
