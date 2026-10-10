open Dependent_records

let (consume @ total) (r : minimum) : {u : unit | r.value <= 0} =
  ghost_ (r.optimality 0);
  let u = () in refine_ u

let (read @ total) (r : interval) : {u : int | r.lower <= u} =
  let {upper; _} = r in upper

let () = assert (read (make 3 7) = 7); assert ((minimum ()).value = 0)
