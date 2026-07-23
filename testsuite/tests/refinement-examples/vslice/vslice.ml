type array
type prophecy : immutable_data
type slice
type snapshot : immutable_data
type final_frame : immutable_data

(* AXIOMATIZED_PROPHECY_SLICE: this file is the complete trusted boundary.
   The model projections and runtime primitives are implemented by the slice
   runtime; their contracts are justified by the unique/local/once discipline
   exposed in [vslice.mli].  No list-algebra or sorting law is assumed here. *)

external contents : array @ local logical -> int list @@ total
  = "vox_vslice_contents"
external current : slice @ local logical -> int list @@ total
  = "vox_vslice_current"
let[@vox.spec_only] (final @ total) :
    slice @ local logical -> int list =
  fun _ -> []

let[@vox.spec_only] (prophecy_value @ total) :
    prophecy @ local logical -> int list =
  fun _ -> []
external snapshot_values : snapshot @ local logical -> int list @@ total
  = "vox_vslice_snapshot_values"
let[@vox.spec_only] (final_frame_values @ total) :
    final_frame @ local logical -> int list =
  fun _ -> []

external make :
  n:int{ 0 <= _ } -> value:int ->
  array{ Vslice_model.len (contents _) = n } @ unique
  = "vox_vslice_make"

external length :
  array:array @ unique ->
  (int{ _ = Vslice_model.len (contents array) }
   * array{ contents _ = contents array }) @ unique
  = "vox_vslice_length"

external get :
  array:array @ unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (contents array)
  } ->
  (int{ _ = Vslice_model.elem (contents array) index }
   * array{ contents _ = contents array }) @ unique
  = "vox_vslice_get"

external new_prophecy : unit -> prophecy @ unique
  = "vox_vslice_new_prophecy"

external borrow :
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
  = "vox_vslice_borrow"

external slice_length :
  loan:slice @ local unique ->
  (int{ _ = Vslice_model.len (current loan) }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique
  = "vox_vslice_length_loan"

external slice_get :
  loan:slice @ local unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (current loan)
  } ->
  (int{ _ = Vslice_model.elem (current loan) index }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique
  = "vox_vslice_get_loan"

external snapshot :
  loan:slice @ local unique ->
  (snapshot{ snapshot_values _ = current loan }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique
  = "vox_vslice_snapshot"

external take_snapshot_values :
  snapshot:snapshot @ local unique ->
  int list{ _ = snapshot_values snapshot } @ unique
  = "vox_vslice_take_snapshot_values"

(* A final frame is a ghost view of the loan's unresolved prophecy, not a
   materialized copy of current array contents.  Its projection is used only
   in refinements to name the stable final value while the loan is threaded. *)
external capture_final_frame :
  loan:slice @ local unique ->
  (final_frame{ final_frame_values _ = final loan }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique
  = "vox_vslice_capture_final_frame"

external slice_set :
  loan:slice @ local unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (current loan)
  } ->
  value:int ->
  slice{
    current _ = Vslice_model.update (current loan) index value
    && final _ = final loan
  } @ local unique
  = "vox_vslice_set_loan"

external split3 :
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
  = "vox_vslice_split3"

external close :
  loan:slice @ local unique -> unit{ final loan = current loan }
  = "%ignore"
