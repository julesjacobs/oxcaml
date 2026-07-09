(* The ghost sort is TRUSTED (declared in gset.mli), so the
   implementation pays no obligation -- it only produces handles.  The
   sort attribute must match the interface (a lean ghost sort is not
   the opaque asymmetry). *)
type t = { mutable c : int } [@@vox.sort lean "GSet"]

let add : (x : int) -> (s : t) -> t{ _ = ins x s } =
  fun x s -> ignore (x, s); assume_unchecked_ { c = 0 }

let member : (x : int) -> (s : t) -> bool{ _ = mem x s } =
  fun x s -> ignore (x, s); assume_unchecked_ false
