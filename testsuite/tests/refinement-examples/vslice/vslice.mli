type array
type prophecy : immutable_data
type slice
type snapshot : immutable_data
type final_frame : immutable_data

val contents : array @ local logical -> int list @@ total
val current : slice @ local logical -> int list @@ total
val final : slice @ local logical -> int list @@ total [@@vox.spec_only]
val prophecy_value : prophecy @ local logical -> int list @@ total
  [@@vox.spec_only]
val snapshot_values : snapshot @ local logical -> int list @@ total
val final_frame_values : final_frame @ local logical -> int list @@ total
  [@@vox.spec_only]

val make :
  n:int{ 0 <= _ } -> value:int ->
  array{ Vslice_model.len (contents _) = n } @ unique

val length :
  array:array @ unique ->
  (int{ _ = Vslice_model.len (contents array) }
   * array{ contents _ = contents array }) @ unique

val get :
  array:array @ unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (contents array)
  } ->
  (int{ _ = Vslice_model.elem (contents array) index }
   * array{ contents _ = contents array }) @ unique

val new_prophecy : unit -> prophecy @ unique

val borrow :
  prophecy:prophecy @ unique ->
  array:array @ unique ->
  local_
    (loan:
       slice{
         current _ = contents array
         && final _ = prophecy_value prophecy
       } @ local unique ->
     'a @ unique) @ once ->
  (array{ contents _ = prophecy_value prophecy } * 'a) @ unique

val slice_length :
  loan:slice @ local unique ->
  (int{ _ = Vslice_model.len (current loan) }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique

val slice_get :
  loan:slice @ local unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (current loan)
  } ->
  (int{ _ = Vslice_model.elem (current loan) index }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique

val snapshot :
  loan:slice @ local unique ->
  (snapshot{ snapshot_values _ = current loan }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique

val take_snapshot_values :
  snapshot:snapshot @ local unique ->
  int list{ _ = snapshot_values snapshot } @ unique

val capture_final_frame :
  loan:slice @ local unique ->
  (final_frame{ final_frame_values _ = final loan }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique

val slice_set :
  loan:slice @ local unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (current loan)
  } ->
  value:int ->
  slice{
    current _ = Vslice_model.update (current loan) index value
    && final _ = final loan
  } @ local unique

val split3 :
  first_prophecy:prophecy @ unique ->
  middle_prophecy:prophecy @ unique ->
  last_prophecy:prophecy @ unique ->
  loan:slice @ local unique ->
  first:int{ 0 <= _ } ->
  last:int{
    first <= _ && _ <= Vslice_model.len (current loan)
  } ->
  local_
    (first_loan:
       slice{
         current _ = Vslice_model.take first (current loan)
         && final _ = prophecy_value first_prophecy
       } @ local unique ->
     middle_loan:
       slice{
         current _ = Vslice_model.segment first last (current loan)
         && final _ = prophecy_value middle_prophecy
       } @ local unique ->
     last_loan:
       slice{
         current _ = Vslice_model.drop last (current loan)
         && final _ = prophecy_value last_prophecy
       } @ local unique ->
     'a @ unique) @ once ->
  (slice{
     current _ =
       Vslice_model.append
         (prophecy_value first_prophecy)
         (Vslice_model.append
            (prophecy_value middle_prophecy)
            (prophecy_value last_prophecy))
     && final _ = final loan
   } * 'a) @ local unique

val close :
  loan:slice @ local unique -> unit{ final loan = current loan }
