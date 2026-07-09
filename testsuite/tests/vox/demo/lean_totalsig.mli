(* A specced interface whose relation parameter is TOTAL ([@vox.total]).
   The marker rides this .cmi, so a client's argument to [apply_step] is
   held to the total (reflectable-only) discipline across the unit
   boundary -- interface honesty.  Positive client: lean_totalclient.ml. *)

[%%vox.lean {lean|
public abbrev IntRel := Int -> Int -> Prop
@[grind, expose] public def rHolds (r : IntRel) (a b : Int) : Prop := r a b
|lean}]

val apply_step :
  (r : ((int -> int -> bool) [@vox.total])) ->
  (f : ((x : int) -> int{ rHolds r x _ })) ->
  (x : int) -> int{ rHolds r x _ }
