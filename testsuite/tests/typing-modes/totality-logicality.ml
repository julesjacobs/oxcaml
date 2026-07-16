(* TEST
   include stdlib_stable;
   flags = "-w -220";
   expect;
*)

let expects_total (f @ total) = f
let use_partial (f @ partial) = f 41
let expects_physical_int (x @ physical) = (x : int)
let expects_physical_ref (x @ physical) = (x : int ref)

(* Positive: arithmetic primitives are total. *)
let increment @ total = fun x -> x + 1
let total_at_partial = use_partial increment
[%%expect{|
val expects_total : 'a @ total -> 'a = <fun>
val use_partial : (int -> 'a) -> 'a = <fun>
val expects_physical_int : int -> int = <fun>
val expects_physical_ref : int ref -> int ref = <fun>
val increment : int -> int = <fun>
val total_at_partial : int = 42
|}]

(* Positive: logicality crossing includes immediates and arrows. *)
let logical_int @ logical = 42
let physical_int = expects_physical_int logical_int
let logical_arrow @ logical = fun x -> x
let _ @ physical = logical_arrow
[%%expect{|
val logical_int : int @@ logical = 42
val physical_int : int = 42
val logical_arrow : 'a -> 'a @@ logical = <fun>
- : 'a -> 'a = <fun>
|}]

(* Negative: refs do not cross logicality. *)
let logical_ref @ logical = ref 0
let _ = expects_physical_ref logical_ref
[%%expect{|
val logical_ref : int ref @@ logical = {contents = 0}
Line 2, characters 29-40:
2 | let _ = expects_physical_ref logical_ref
                                 ^^^^^^^^^^^
Error: This value is "logical" but is expected to be "physical".
|}]

let _ = !logical_ref
[%%expect{|
Line 1, characters 9-20:
1 | let _ = !logical_ref
             ^^^^^^^^^^^
Error: This value is "logical" but is expected to be "physical".
|}]

(* Atomic values deliberately differ from contended values: they do not cross
   logicality, while atomic access remains permitted at contended. *)
let logical_atomic @ logical = Atomic.make 0
let _ = Atomic.get logical_atomic
[%%expect{|
val logical_atomic : int Atomic.t @@ logical = {Atomic.contents = 0}
Line 2, characters 19-33:
2 | let _ = Atomic.get logical_atomic
                       ^^^^^^^^^^^^^^
Error: This value is "logical" but is expected to be "physical".
|}]

let get_contended_atomic : int Atomic.t @ contended -> int =
  fun x -> Atomic.get x
let physical_atomic = Atomic.make 0
let _ = get_contended_atomic physical_atomic
[%%expect{|
val get_contended_atomic : int Atomic.t @ contended -> int = <fun>
val physical_atomic : int Atomic.t = {Atomic.contents = 0}
- : int = 0
|}]

(* Positive: refs cross totality.  A total closure may mention, store, and
   return a captured ref; Lock only prevents eliminating its logical view. *)
let captured_ref = ref 0
let return_captured_ref @ total = fun () -> captured_ref
let store_captured_ref @ total = fun () -> Some captured_ref
[%%expect{|
val captured_ref : int ref = {contents = 0}
val return_captured_ref : unit -> int ref @ logical = <fun>
val store_captured_ref : unit -> int ref option @ logical = <fun>
|}]

type mixed_record = { immutable_field : int; mutable mutable_field : int }
let captured_record = { immutable_field = 42; mutable_field = 0 }
let project_immutable @ total = fun () -> captured_record.immutable_field
[%%expect{|
type mixed_record = { immutable_field : int; mutable mutable_field : int; }
val captured_record : mixed_record =
  {immutable_field = 42; mutable_field = 0}
val project_immutable : unit -> int = <fun>
|}]

(* Negative: Lock applies to captured and passed refs. *)
let _ @ total = fun () -> !captured_ref
[%%expect{|
Line 1, characters 26-27:
1 | let _ @ total = fun () -> !captured_ref
                              ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-39
         which is expected to be "total".
|}]

let _ @ total = fun () -> captured_ref := 1
[%%expect{|
Line 1, characters 39-41:
1 | let _ @ total = fun () -> captured_ref := 1
                                           ^^
Error: The value "(:=)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-43
         which is expected to be "total".
|}]

let _ @ total = fun (r : int ref) -> !r
[%%expect{|
Line 1, characters 37-38:
1 | let _ @ total = fun (r : int ref) -> !r
                                         ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-39
         which is expected to be "total".
|}]

let _ @ total = fun (r : int ref) -> r := 1
[%%expect{|
Line 1, characters 39-41:
1 | let _ @ total = fun (r : int ref) -> r := 1
                                           ^^
Error: The value "(:=)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-43
         which is expected to be "total".
|}]

(* Boundary: captures of genuinely-partial arrow-typed values are rejected in
   batch compilation as well as in toplevel phrases. *)
let capture_annotated_partial (g @ partial) =
  let _closure @ total = fun () -> g in
  ()
[%%expect{|
Line 2, characters 35-36:
2 |   let _closure @ total = fun () -> g in
                                       ^
Error: The value "g" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 25-36
         which is expected to be "total".
|}]

let capture_impure_closure () =
  let impure = fun x -> ignore (ref x); x in
  let _closure @ total = fun () -> impure in
  ()
[%%expect{|
Line 3, characters 35-41:
3 |   let _closure @ total = fun () -> impure in
                                       ^^^^^^
Error: The value "impure" is "partial"
         because it closes over the value "ignore" at line 2, characters 24-30
         which is "partial".
       However, the value "impure" highlighted is expected to be "total"
         because it is used inside the function at line 3, characters 25-41
         which is expected to be "total".
|}]

let capture_partial_stored (g @ partial) =
  let _closure @ total = fun () -> Some g in
  ()
[%%expect{|
Line 2, characters 40-41:
2 |   let _closure @ total = fun () -> Some g in
                                            ^
Error: The value "g" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 25-41
         which is expected to be "total".
|}]

(* This companion positive documents the batch/toplevel divergence.  Defining
   and capturing [pure] in one phrase preserves its inferred totality.  Across
   an [%%expect] phrase boundary the harness would zap it to legacy partial;
   that is a harness artifact, not a Boundary check. *)
let capture_inferred_total () =
  let pure = fun x -> x in
  let _closure @ total = fun () -> pure in
  ()
[%%expect{|
val capture_inferred_total : unit -> unit = <fun>
|}]

let _ @ total = fun (f @ partial) -> f
[%%expect{|
Line 1, characters 16-38:
1 | let _ @ total = fun (f @ partial) -> f
                    ^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

(* Hereditary: nested function literals in total bodies must be total. *)
let _ @ total =
  fun () ->
    let local @ partial = fun x -> x in
    local
[%%expect{|
Lines 3-4, characters 4-9:
3 | ....let local @ partial = fun x -> x in
4 |     local
Error: The function is "partial" but is expected to be "total".
|}]

(* Rec: harmless recursion crosses or earns totality. *)
let nonrecursive_rec @ total =
  let rec f = fun x -> x in
  (f : _ @ total)

let cyclic_list @ total =
  let rec xs = 1 :: xs in
  (xs : _ @ total)
[%%expect{|
val nonrecursive_rec : 'a -> 'a = <fun>
val cyclic_list : int list = [1; <cycle>]
|}]

(* Rec: recursive functions are partial inside their RHS and afterwards. *)
let rec recursive_inside @ total = fun x -> recursive_inside x
[%%expect{|
Line 1, characters 44-60:
1 | let rec recursive_inside @ total = fun x -> recursive_inside x
                                                ^^^^^^^^^^^^^^^^
Error: The value "recursive_inside" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 35-62
         which is expected to be "total".
|}]

let rec recursive_after = fun x -> recursive_after x
let _ = expects_total recursive_after
[%%expect{|
val recursive_after : 'a -> 'b = <fun>
Line 2, characters 22-37:
2 | let _ = expects_total recursive_after
                          ^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let rec ops = ((fun x -> (fst ops) x), 0)
let _ = expects_total (fst ops)
[%%expect{|
val ops : ('a -> 'b) * int = (<fun>, 0)
Line 2, characters 22-31:
2 | let _ = expects_total (fst ops)
                          ^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let rec even n = if n = 0 then true else odd (n - 1)
and odd n = if n = 0 then false else even (n - 1)
[%%expect{|
val even : int -> bool = <fun>
val odd : int -> bool = <fun>
|}]

let _ = expects_total even
[%%expect{|
Line 1, characters 22-26:
1 | let _ = expects_total even
                          ^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let _ = expects_total odd
[%%expect{|
Line 1, characters 22-25:
1 | let _ = expects_total odd
                          ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* Ops: impure primitives and externals are partial values at the boundary. *)
let _ @ total = fun x -> ref x
[%%expect{|
Line 1, characters 25-28:
1 | let _ @ total = fun x -> ref x
                             ^^^
Error: The value "ref" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-30
         which is expected to be "total".
|}]

let _ @ total = fun () -> raise Exit
[%%expect{|
Line 1, characters 26-31:
1 | let _ @ total = fun () -> raise Exit
                              ^^^^^
Error: The value "raise" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-36
         which is expected to be "total".
|}]

let _ @ total = fun () -> print_string "not total"
[%%expect{|
Line 1, characters 26-38:
1 | let _ @ total = fun () -> print_string "not total"
                              ^^^^^^^^^^^^
Error: The value "print_string" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-50
         which is expected to be "total".
|}]

let _ @ total = fun (f : int -> int) -> f == f
[%%expect{|
Line 1, characters 42-44:
1 | let _ @ total = fun (f : int -> int) -> f == f
                                              ^^
Error: This expression is not allowed at mode total: physical comparison on non-immediate values is not permitted in total code.
|}]

let _ @ total = fun () -> while false do () done
[%%expect{|
Line 1, characters 26-48:
1 | let _ @ total = fun () -> while false do () done
                              ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed at mode total: loops are not permitted in total code.
|}]

(* Exception constructs themselves are allowed. *)
let try_is_allowed @ total = fun () -> try 1 with _ -> 2
let local_exception_is_allowed @ total =
  fun () ->
    let exception Local_exception in
    0
[%%expect{|
val try_is_allowed : unit -> int = <fun>
val local_exception_is_allowed : unit -> int = <fun>
|}]
