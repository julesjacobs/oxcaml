(* Contracts flow through NESTED calls -- no let-binding needed.  Each
   argument value gets a name from the verifier itself (the [*arg*] in
   the obligation), exactly as a manual [let tmp = bump n in ...] would
   have provided: [bump]'s result contract feeds the next call's
   precondition directly, twice over. *)

let bump (x : int) : int{ _ >= x + 1 } = x + 1

(* A precondition to discharge at every call site: y >= 2. *)
let shrink (y : int{ y >= 2 }) : int{ _ >= 1 } = y - 1

(* Two nested [bump]s establish shrink's precondition from n >= 0:
   bump n >= n + 1 >= 1, and bump (bump n) >= 2.  Put the cursor on the
   call to watch the [*arg*] obligations. *)
let use (n : int{ n >= 0 }) : int{ _ >= 1 } = shrink (bump (bump n))
