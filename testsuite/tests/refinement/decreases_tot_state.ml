(* A measure descends only if the verifier can read it at two different
   times and get the two values the program gets.  Everything here is
   truthfully total -- reading and writing a mutable field both terminate --
   and the state is behind an [int -> int] interface, so no mutable value
   ever reaches an obligation.  [f 0 1 1] still runs forever, because the
   [set] between the test and the call changes what [get j] answers. *)
let (expects_total @ total) (g @ total) = g

let a = ref 0
let b = ref 0
let cell (i : int) : int ref = if i = 0 then a else b
let get (i : int) : int = (cell i).contents
let set (i : int) (x : int) : unit = (cell i).contents <- x

let[@vox.decreases n] rec f (i : int) (j : int) (n : int) : int =
  if get j >= 0 && get j < n
  then begin
    set j (n + 1);
    f j i (get j)
  end
  else 0

let total_use () = expects_total f
