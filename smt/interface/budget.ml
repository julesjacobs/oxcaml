(* Deterministic monotone effort budget. See budget.mli.

   Stdlib-only. A trivial mutable cell: the whole point is that the increment is cheap
   enough to sit on the SAT conflict/decision hot path with no measurable cost, and that
   the count is a pure function of the (deterministic) search, so it is reproducible. *)

type t =
  { mutable used : int
  ; mutable max : int option
  }

exception Exceeded

let create ?max () = { used = 0; max }

let tick t =
  t.used <- t.used + 1;
  match t.max with
  | Some m when t.used > m -> raise Exceeded
  | _ -> ()
;;

let used t = t.used
let reset t = t.used <- 0

let with_cap t cap f =
  let saved = t.max in
  let effective =
    match saved with
    | None -> cap
    | Some configured -> min configured cap
  in
  t.max <- Some effective;
  Fun.protect ~finally:(fun () -> t.max <- saved) f
;;
