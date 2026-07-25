(* The fallback can only name the remaining constructor by ruling out the two
   arms above it, one of which carries a payload.  Nothing in scope states the
   conclusion, so the arm facts have to carry their meaning: left as opaque
   symbols the payload-carrying constructor cannot be excluded. *)
type choice =
  | Choice_a of int
  | Choice_b
  | Choice_c

let essential (value : choice) =
  match value with
  | Choice_a _ -> Choice_c
  | Choice_b -> Choice_c
  | _ -> (value : choice{ _ = Choice_c })

(* One constructor used at two instantiations.  While the arm fact was
   registered as a symbol keyed by constructor name these collided and were
   rejected as used at inconsistent types. *)
type 'a box =
  | Full of 'a
  | Spare
  | Void

let polymorphic (x : int box) (y : bool box) =
  match x with
  | Full _ -> Spare
  | Void -> Spare
  | _ ->
    (match y with
     | Full _ -> Spare
     | Void -> Spare
     | _ -> (y : bool box{ _ = Spare }))
