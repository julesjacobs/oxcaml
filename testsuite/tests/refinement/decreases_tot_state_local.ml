(* The same loop as [decreases_tot_state.ml], with the reads bound to locals
   first.  The argument at the measured position is then a plain identifier,
   which the verifier can compare; what it may not do is carry the value it
   observed through [get] across the write that follows.  [f 0 1 1] runs
   forever here too. *)
let (expects_total @ total) (g @ total) = g

let a = ref 0
let b = ref 0
let cell (i : int) : int ref = if i = 0 then a else b
let get (i : int) : int = (cell i).contents
let set (i : int) (x : int) : unit = (cell i).contents <- x

let[@vox.decreases n] rec f (i : int) (j : int) (n : int) : int =
  let x = get j in
  if x >= 0 && x < n
  then begin
    set j (n + 1);
    let y = get j in
    f j i y
  end
  else 0

let total_use () = expects_total f
