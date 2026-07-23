type array =
  { view : int list;
    base : int Stdlib.Array.t
  }

type prophecy = { value : int list } [@@boxed]

type slice =
  { view : int list;
    final_value : int list;
    base : int Stdlib.Array.t;
    offset : int;
    length : int
  }

type snapshot = int list

type final_frame = { value : int list } [@@boxed]

(* This is the only trusted operational cast in this module.  It attaches the
   abstract ghost contracts and unique mode to concrete runtime values.  Every
   call either wraps a fresh allocation or consumes the value it reissues. *)
external trusted_refinement_cast :
  ('a[@local_opt]) @ unique -> ('b[@local_opt]) @ unique
  = "%identity"

external contents : array @ local logical -> int list @@ total
  = "%field0"
external current : slice @ local logical -> int list @@ total
  = "%field0"

(* These projections describe values that are only known when their loan is
   closed.  Their bodies are inaccessible at value level and deliberately do
   not provide a runtime representation for the future values. *)
let[@vox.spec_only] (final @ total) :
    slice @ local logical -> int list =
  fun _ -> []

let[@vox.spec_only] (prophecy_value @ total) :
    prophecy @ local logical -> int list =
  fun _ -> []

let[@vox.spec_only] (final_frame_values @ total) :
    final_frame @ local logical -> int list =
  fun _ -> []

external snapshot_values : snapshot @ local logical -> int list @@ total
  = "%identity"

let rec list_of_segment base index remaining accumulator =
  if remaining = 0
  then List.rev accumulator
  else
    list_of_segment base (index + 1) (remaining - 1)
      (Array.get base index :: accumulator)

(* [snapshot] may allocate its list in the loan's local region.  Clients of
   [take_snapshot_values] receive a global list, so copy every cons cell
   instead of changing the mode of the local list in place. *)
let rec globalize_int_list (local_ values) =
  match values with
  | [] -> []
  | value :: rest -> (value + 0) :: globalize_int_list rest

let rec repeat count value accumulator =
  if count = 0
  then accumulator
  else repeat (count - 1) value (value :: accumulator)

let rec take_view count values =
  if count <= 0
  then []
  else
    match values with
    | [] -> []
    | head :: tail -> head :: take_view (count - 1) tail

let rec drop_view count values =
  if count <= 0
  then values
  else
    match values with
    | [] -> []
    | _ :: tail -> drop_view (count - 1) tail

(* The verifier needs an implementation-side inhabitant for the abstract
   standard-array field while checking the concrete representation. *)
let empty_array_for_logic : int Stdlib.Array.t = [||]

let make :
  n:int{ 0 <= _ } -> value:int ->
  array{ Vslice_model.len (contents _) = n } @ unique =
  fun ~n ~value ->
    trusted_refinement_cast
      (Obj.magic_unique
         ({ view = repeat n value [];
            base = Array.make n value
          } : array))

let length :
  array:array @ unique ->
  (int{ _ = Vslice_model.len (contents array) }
   * array{ contents _ = contents array }) @ unique =
  fun ~(array : array) ->
    let ({ view; base } : array) = array in
    trusted_refinement_cast
      (Obj.magic_unique (Array.length base, { view; base }))

let get :
  array:array @ unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (contents array)
  } ->
  (int{ _ = Vslice_model.elem (contents array) index }
   * array{ contents _ = contents array }) @ unique =
  fun ~(array : array) ~index ->
    let ({ view; base } : array) = array in
    trusted_refinement_cast
      (Obj.magic_unique (Array.get base index, { view; base }))

let new_prophecy : unit -> prophecy @ unique =
  fun () ->
    trusted_refinement_cast
      (Obj.magic_unique ({ value = [] } : prophecy))

let borrow :
  prophecy:prophecy @ unique ->
  array:array @ unique ->
  local_
    (loan:
       slice{
         current _ = contents array
         && final _ = prophecy_value prophecy
       } @ local unique ->
     'a @ unique) @ once ->
  (array{ contents _ = prophecy_value prophecy } * 'a) @ unique =
  trusted_refinement_cast
    (Obj.magic_unique
       (fun ~(prophecy : prophecy) ~(array : array) continuation ->
         let ({ value = final_value } : prophecy) = prophecy in
         let ({ view; base } : array) = array in
         (* Fork-join clients may transfer a closure that captures this loan.
            Keep the concrete record, and the split records below, outside the
            caller's local region so the transfer never retains local data. *)
         let loan : slice @ global =
           { view;
             final_value;
             base;
             offset = 0;
             length = Array.length base
           }
         in
         let result = continuation ~loan in
         let view = list_of_segment base 0 (Array.length base) [] in
         { view; base }, result))

let slice_length :
  loan:slice @ local unique ->
  (int{ _ = Vslice_model.len (current loan) }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique =
  fun ~(loan : slice) ->
    let { length; _ } = loan in
    exclave_ trusted_refinement_cast (Obj.magic_unique (length, loan))

let slice_get :
  loan:slice @ local unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (current loan)
  } ->
  (int{ _ = Vslice_model.elem (current loan) index }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique =
  fun ~(loan : slice) ~index ->
    let { base; offset; _ } = loan in
    exclave_ trusted_refinement_cast
      (Obj.magic_unique (Array.get base (offset + index), loan))

let snapshot :
  loan:slice @ local unique ->
  (snapshot{ snapshot_values _ = current loan }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique =
  fun ~(loan : slice) ->
    let { base; offset; length; _ } = loan in
    let values = list_of_segment base offset length [] in
    exclave_ trusted_refinement_cast (Obj.magic_unique (values, loan))

let take_snapshot_values :
  snapshot:snapshot @ local unique ->
  int list{ _ = snapshot_values snapshot } @ unique =
  fun ~(snapshot : snapshot) ->
    trusted_refinement_cast
      (Obj.magic_unique (globalize_int_list snapshot))

let capture_final_frame :
  loan:slice @ local unique ->
  (final_frame{ final_frame_values _ = final loan }
   * slice{
       current _ = current loan
       && final _ = final loan
     }) @ local unique =
  fun ~(loan : slice) ->
    let { final_value; _ } = loan in
    exclave_ trusted_refinement_cast
      (Obj.magic_unique
         (({ value = final_value } : final_frame), loan))

let slice_set :
  loan:slice @ local unique ->
  index:int{
    0 <= _ && _ < Vslice_model.len (current loan)
  } ->
  value:int ->
  slice{
    current _ = Vslice_model.update (current loan) index value
    && final _ = final loan
  } @ local unique =
  trusted_refinement_cast
    (Obj.magic_unique
       (fun ~(loan : slice) ~index ~value ->
         let fields = loan in
         Array.set fields.base (fields.offset + index) value;
         { fields with
           view = Vslice_model.update (current fields) index value
         }))

let split3 :
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
   } * 'a) @ local unique =
  trusted_refinement_cast
    (Obj.magic_unique
       (fun ~(first_prophecy : prophecy)
          ~(middle_prophecy : prophecy) ~(last_prophecy : prophecy)
          ~(loan : slice)
          ~first ~last continuation ->
         let ({ value = first_final } : prophecy) = first_prophecy in
         let ({ value = middle_final } : prophecy) = middle_prophecy in
         let ({ value = last_final } : prophecy) = last_prophecy in
         let parent = loan in
         let first_loan : slice @ global =
           { view = take_view first parent.view;
             final_value = first_final;
             base = parent.base;
             offset = parent.offset;
             length = first
           }
         in
         let middle_loan : slice @ global =
           { view =
               take_view (last - first)
                 (drop_view first parent.view);
             final_value = middle_final;
             base = parent.base;
             offset = parent.offset + first;
             length = last - first
           }
         in
         let last_loan : slice @ global =
           { view = drop_view last parent.view;
             final_value = last_final;
             base = parent.base;
             offset = parent.offset + last;
             length = parent.length - last
           }
         in
         let result =
           continuation ~first_loan ~middle_loan ~last_loan
         in
         let view =
           list_of_segment parent.base parent.offset parent.length []
         in
         { parent with view }, result))

let close :
  loan:slice @ local unique -> unit{ final loan = current loan } =
  trusted_refinement_cast
    (Obj.magic_unique
       (fun ~(loan : slice) ->
         let _ = loan in
         ()))
