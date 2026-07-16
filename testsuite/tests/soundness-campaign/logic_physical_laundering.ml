(* TEST include stdlib_stable; flags = "-w -220"; expect;
*)

(* SOUNDNESS CAMPAIGN — Family 1 (logicality: physical access to logical values).

   v2 rename: the logicality axis is now physical/logical (was program/logic).
   A [logical] value is one you hold only logical access to (its denotation, not
   its representation). Physical access — mutation, or flowing it back into a
   [physical] position — must be blocked, at every projection and elimination.

   v2 CROSSING: like portability, logicality has crossing — immediates and
   arrows cross logicality (testsuite/tests/typing-modes/totality-logicality.ml).
   A logical [int] may therefore flow to a physical position (X0): this is
   SOUND, an immediate has no physical representation to launder. The meaningful
   laundering probes must use a NON-crossing carrier; [ref] does not cross
   logicality, so L1-L5 use a logical [int ref] and confirm its physical content
   cannot reach a physical position through any projection/elimination. (The
   first sweep used a logical [int] and predated crossing; those accepts were
   the crossing behavior, not a leak.) *)

(* X0: a logical immediate crosses to physical — SOUND (documents crossing). *)
let logical_int @ logical = 42
let crossed @ physical = logical_int
[%%expect
  {|
val logical_int : int @@ logical = 42
val crossed : int = 42
|}]

(* Non-crossing carrier for the laundering probes. *)
let logical_ref @ logical = ref 0
[%%expect
  {|
val logical_ref : int ref @@ logical = {contents = 0}
|}]

(* L1: launder the logical ref to physical through a tuple projection. *)
let leaked1 @ physical = fst (logical_ref, 0)
[%%expect
  {|
Line 1, characters 30-41:
1 | let leaked1 @ physical = fst (logical_ref, 0)
                                  ^^^^^^^^^^^
Error: This value is "logical"
       but is expected to be "physical"
         because it is an element of the tuple at line 1, characters 29-45
         which is expected to be "physical".
|}]

(* L2: launder through a record field. *)
type box = { v : int ref }

let boxed = { v = logical_ref }
let leaked2 @ physical = boxed.v
[%%expect
  {|
type box = { v : int ref; }
val boxed : box @@ logical = {v = {contents = 0}}
Line 4, characters 25-32:
4 | let leaked2 @ physical = boxed.v
                             ^^^^^^^
Error: This value is "logical"
         because it is the field "v" of the record at line 4, characters 25-30
         which is "logical"
         because it is a record whose field "v" is the expression at line 3, characters 18-29
         which is "logical".
       However, the highlighted expression is expected to be "physical".
|}]

(* L3: launder through a data constructor and match. *)
let opt = Some logical_ref

let leaked3 @ physical =
  match opt with
  | Some x -> x
  | None -> ref 0
;;
[%%expect
  {|
val opt : int ref option @@ logical = Some {contents = 0}
Line 5, characters 14-15:
5 |   | Some x -> x
                  ^
Error: This value is "logical"
         because it is contained (via constructor "Some") in the value at line 5, characters 4-10
         which is "logical"
         because it contains (via constructor "Some") the expression at line 1, characters 15-26
         which is "logical".
       However, the highlighted expression is expected to be "physical".
|}]

(* L4: pass the logical ref through a total identity and demand physical out. *)
let (id @ total) x = x
let leaked4 @ physical = id logical_ref
[%%expect
  {|
val id : 'a -> 'a = <fun>
Line 2, characters 28-39:
2 | let leaked4 @ physical = id logical_ref
                                ^^^^^^^^^^^
Error: This value is "logical" but is expected to be "physical".
|}]

(* L5: physical (mutable) access to the logical ref. Dereference is a physical
   access and must be blocked. *)
let leaked5 = !logical_ref
[%%expect
  {|
Line 1, characters 15-26:
1 | let leaked5 = !logical_ref
                   ^^^^^^^^^^^
Error: This value is "logical" but is expected to be "physical".
|}]
