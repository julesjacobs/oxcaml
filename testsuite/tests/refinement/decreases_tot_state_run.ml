(* The reviewer's runnable witness, kept as it was written.

   It is [decreases_tot_state.ml] with a main: compiled by the lane compiler
   it links, prints its first line, and never prints the second -- the run
   was still going at an eight-second timeout.  Every function here is
   truthfully total, and the measure is [n], in last position, so nothing
   about it depends on how the arguments are substituted.  What it depends on
   is [get j] answering the same thing before and after [set j (n + 1)], and
   the descent is refused for saying so. *)
let (expects_total @ total) (g @ total) = g
let a = ref 0
let b = ref 0
let cell (i : int) : int ref = if i = 0 then a else b
let get (i : int) : int = (cell i).contents
let set (i : int) (x : int) : unit = (cell i).contents <- x

let[@vox.decreases n] rec f (i : int) (j : int) (n : int) : int =
  if get j >= 0 && get j < n then begin
    set j (n + 1);
    f j i (get j)
  end else 0

let _ = expects_total f
let () =
  print_string "entering f 0 1 1\n"; flush stdout;
  ignore (f 0 1 1);
  print_string "f returned\n"
