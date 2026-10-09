(* The shared backtracking substrate (ADR-0014 Stage 0). See trail.mli. Stdlib-only, per
   the oxsmt_core dependency firewall (AGENTS.md I3). *)

type ('e, 'f) t =
  { entries : 'e Dynarray.t
  ; (* One per open frame, in push order: [(watermark, payload)] where [watermark] is the
       trail length at the moment the frame was opened and [payload] is the caller state
       to restore to should the frame become a [pop] target. *)
    frames : (int * 'f) Dynarray.t
  }

let create () = { entries = Dynarray.create (); frames = Dynarray.create () }
let length t = Dynarray.length t.entries
let depth t = Dynarray.length t.frames
let record t e = Dynarray.add_last t.entries e
let push t f = Dynarray.add_last t.frames (Dynarray.length t.entries, f)
let mark t = Dynarray.length t.entries

(* Drain newest-first to trail length [target], applying each entry's inverse. The single
   place the pop watermark loop lives. *)
let drain_to t ~apply target =
  while Dynarray.length t.entries > target do
    apply (Dynarray.pop_last t.entries)
  done
;;

let rewind_to t ~apply m =
  if m < 0 || m > Dynarray.length t.entries then invalid_arg "Trail.rewind_to";
  drain_to t ~apply m
;;

let pop t ~apply ?(restore = ignore) n =
  if n < 0 then invalid_arg "Trail.pop: negative";
  if n > Dynarray.length t.frames then invalid_arg "Trail.pop: too many frames";
  if n > 0
  then (
    let target = Dynarray.length t.frames - n in
    let watermark, payload = Dynarray.get t.frames target in
    drain_to t ~apply watermark;
    restore payload;
    Dynarray.truncate t.frames target)
;;

module For_test = struct
  let entries t = Dynarray.to_list t.entries
end
