(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* SOUNDNESS CAMPAIGN — Family 5 (termination-specific laundering).

   Totality's one addition over statelessness is forbidding non-termination. (Rec): inside
   a let-rec group's RHS the group variables are partial, so a self- or mutually-recursive
   function is partial. A [total] value must denote a terminating mathematical function.
   Any construction that yields a [total] value which can diverge is a soundness finding. *)

let expects_total (f @ total) = f

(* T1: self-recursion hidden behind an eta-expansion, forced total afterwards. (Rec) must
   make the whole thing partial. *)
let rec spin x = spin x
let escaped_spin = expects_total spin

[%%expect
  {|
val expects_total : 'a @ total -> 'a = <fun>
val spin : 'a -> 'b = <fun>
Line 6, characters 33-37:
6 | let escaped_spin = expects_total spin
                                     ^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* T2: corecursion via lazy — a diverging thunk forced inside a total closure. *)
let rec ones = lazy (1 :: Lazy.force ones)
let (force_ones @ total) () = Lazy.force ones

[%%expect
  {|
val ones : int list Lazy.t = <lazy>
Line 2, characters 30-40:
2 | let (force_ones @ total) () = Lazy.force ones
                                  ^^^^^^^^^^
Error: The value "Lazy.force" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 25-45
         which is expected to be "total".
|}]

(* T3: mutual recursion, force one member total afterwards. *)
let rec ping x = pong x
and pong x = ping x

let escaped_ping = expects_total ping

[%%expect
  {|
val ping : 'a -> 'b = <fun>
val pong : 'a -> 'b = <fun>
Line 4, characters 33-37:
4 | let escaped_ping = expects_total ping
                                     ^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* T4: recursion laundered through a data structure then extracted. The spec's canonical
   [let rec ops = ((fun x -> (fst ops) x), 0)] example, forced total. *)
let rec ops = (fun x -> (fst ops) x), 0
let escaped_ops = expects_total (fst ops)

[%%expect
  {|
val ops : ('a -> 'b) * int = (<fun>, 0)
Line 2, characters 32-41:
2 | let escaped_ops = expects_total (fst ops)
                                    ^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]
