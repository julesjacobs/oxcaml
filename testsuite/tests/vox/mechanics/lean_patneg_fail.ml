(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* SOUNDNESS: the deep-pattern negative of a multi-scrutinee arm is the
   negation of the CONJUNCTION over its pinned components -- every pin
   must be kept.  Here the earlier arm [B, Nd (R, k)] pins the colour to
   [B] and leaves [k] free.  The catch-all handles [(R, Nd (R, k))] as
   [2], but the model [g] gives [3] there, so the function is NOT the
   model and MUST be rejected.  It would be wrongly ACCEPTED only if the
   negative dropped the [c = B] pin (becoming the too-strong "v is never
   Nd (R, _)"), which would eliminate the [(R, Nd (R, k))] model case.
   The sound negative keeps the pin, so that case survives and the
   equality fails. *)

type col = R | B
type t = Lf | Nd of col * int
[%%expect{|
type col = R | B
type t = Lf | Nd of col * int
|}]

[%%vox.lean {lean|
@[grind, expose] def g : Vox_col -> Vox_t -> Int
  | .B, .Nd .R _k => 1
  | .R, .Nd .R _k => 3
  | _, _ => 2
|lean}]
[%%expect{|
|}]

let f (c : col) (v : t) : int{ _ = g c v } =
  match c, v with
  | B, Nd (R, k) -> 1
  | _c2, _v2 -> 2
[%%expect{|
Line 4, characters 16-17:
4 |   | _c2, _v2 -> 2
                    ^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: 2 = g c v
Hypotheses:
  _c2 = fst (c, v)
  _v2 = snd (c, v)
  not (fst (c, v) = B && (exists_ *vox-ex*. snd (c, v) = Nd (R, *vox-ex*)))
(lean: error: `grind` failed)
|}]
