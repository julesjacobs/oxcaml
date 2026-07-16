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

(* Boundary spec-bug fix (ruled 2026-07-16): a total closure may declare a
   partial arrow-typed PARAMETER.  Parameters are left alone, exactly as
   portability/contention leave them; only captures are constrained.  So this
   is accepted. *)
let _ @ total = fun (f @ partial) -> f
[%%expect{|
- : 'a -> 'a = <fun>
|}]

(* Structure-boundary guards: these unannotated top-level functions default to
   partial before their later use at total.  They do not test late inference. *)
let late_partial_parameter (ignored @ partial) = 0
let _ = expects_total late_partial_parameter
[%%expect{|
val late_partial_parameter : 'a -> int = <fun>
Line 2, characters 22-44:
2 | let _ = expects_total late_partial_parameter
                          ^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let late_partial_local () =
  let _local @ partial = fun x -> x in
  0
let _ = expects_total late_partial_local
[%%expect{|
val late_partial_local : unit -> int = <fun>
Line 4, characters 22-40:
4 | let _ = expects_total late_partial_local
                          ^^^^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let late_array () = [| 0 |]
let _ = expects_total late_array
[%%expect{|
val late_array : unit -> int array = <fun>
Line 2, characters 22-32:
2 | let _ = expects_total late_array
                          ^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let late_mutable_record () = { immutable_field = 0; mutable_field = 0 }
let _ = expects_total late_mutable_record
[%%expect{|
val late_mutable_record : unit -> mixed_record = <fun>
Line 2, characters 22-41:
2 | let _ = expects_total late_mutable_record
                          ^^^^^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let late_loop () = while false do () done
let _ = expects_total late_loop
[%%expect{|
val late_loop : unit -> unit = <fun>
Line 2, characters 22-31:
2 | let _ = expects_total late_loop
                          ^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* Late-inference regression guard (spec Ops): a residue construct inside a
   let-in closure whose totality is still an inference variable, then forced
   total by use, must be REJECTED.  The rejection is a submode edge on the
   closure's totality variable, never a snapshot of a momentary context; the
   let-in position (unlike a top-level binding) has no structure boundary to
   fall back on, so this is exactly the case a snapshot check would leak. *)
let _ = let bad () = while true do () done in expects_total bad
[%%expect{|
Line 1, characters 60-63:
1 | let _ = let bad () = while true do () done in expects_total bad
                                                                ^^^
Error: This value is "partial" but is expected to be "total".
|}]

let _ = let bad () = for _i = 0 to 1 do () done in expects_total bad
[%%expect{|
Line 1, characters 65-68:
1 | let _ = let bad () = for _i = 0 to 1 do () done in expects_total bad
                                                                     ^^^
Error: This value is "partial" but is expected to be "total".
|}]

let _ = let bad () = { immutable_field = 0; mutable_field = 0 } in
        expects_total bad
[%%expect{|
Line 2, characters 22-25:
2 |         expects_total bad
                          ^^^
Error: This value is "partial" but is expected to be "total".
|}]

let _ = let bad () = [| 0 |] in expects_total bad
[%%expect{|
Line 1, characters 46-49:
1 | let _ = let bad () = [| 0 |] in expects_total bad
                                                  ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* Statement-position residue inside a late-inferred closure (sequence LHS and
   discarded local let) must also be rejected: the closure's totality is still
   an inference variable when the residue is typed and is pinned total only
   later by use, so the constraint is a submode edge on that variable, never a
   snapshot. *)
let _ = let bad () = ((while true do () done); 0) in expects_total bad
[%%expect{|
Line 1, characters 67-70:
1 | let _ = let bad () = ((while true do () done); 0) in expects_total bad
                                                                       ^^^
Error: This value is "partial" but is expected to be "total".
|}]

let _ = let bad () = (let _ = while true do () done in 0) in expects_total bad
[%%expect{|
Line 1, characters 75-78:
1 | let _ = let bad () = (let _ = while true do () done in 0) in expects_total bad
                                                                               ^^^
Error: This value is "partial" but is expected to be "total".
|}]

let _ = let bad () = (let _ = [| 0 |] in 0) in expects_total bad
[%%expect{|
Line 1, characters 61-64:
1 | let _ = let bad () = (let _ = [| 0 |] in 0) in expects_total bad
                                                                 ^^^
Error: This value is "partial" but is expected to be "total".
|}]

let _ = let bad () = (let _ = { immutable_field = 0; mutable_field = 0 } in 0) in expects_total bad
[%%expect{|
Line 1, characters 96-99:
1 | let _ = let bad () = (let _ = { immutable_field = 0; mutable_field = 0 } in 0) in expects_total bad
                                                                                                    ^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* The tail-position row of the residue matrix is covered immediately above.
   The remaining rows require the enclosing closure's live totality variable
   even though the residue's value is discarded. *)
let bad_while_sequence @ total =
  fun () -> (while true do () done); 0
[%%expect{|
Line 2, characters 12-35:
2 |   fun () -> (while true do () done); 0
                ^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

let bad_while_discard @ total =
  fun () -> let _ = while true do () done in 0
[%%expect{|
Line 2, characters 20-41:
2 |   fun () -> let _ = while true do () done in 0
                        ^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

let bad_for_sequence @ total =
  fun () -> (for _i = 0 to 1 do () done); 0
[%%expect{|
Line 2, characters 12-40:
2 |   fun () -> (for _i = 0 to 1 do () done); 0
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

let bad_for_discard @ total =
  fun () -> let _ = for _i = 0 to 1 do () done in 0
[%%expect{|
Line 2, characters 20-46:
2 |   fun () -> let _ = for _i = 0 to 1 do () done in 0
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

let bad_array_sequence @ total = fun () -> [| 0 |]; 0
[%%expect{|
Line 1, characters 43-50:
1 | let bad_array_sequence @ total = fun () -> [| 0 |]; 0
                                               ^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

let bad_array_discard @ total = fun () -> let _ = [| 0 |] in 0
[%%expect{|
Line 1, characters 50-57:
1 | let bad_array_discard @ total = fun () -> let _ = [| 0 |] in 0
                                                      ^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

let bad_mutable_record_sequence @ total =
  fun () -> { immutable_field = 0; mutable_field = 0 }; 0
[%%expect{|
Line 2, characters 12-54:
2 |   fun () -> { immutable_field = 0; mutable_field = 0 }; 0
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

let bad_mutable_record_discard @ total =
  fun () ->
    let _ = { immutable_field = 0; mutable_field = 0 } in
    0
[%%expect{|
Line 3, characters 12-54:
3 |     let _ = { immutable_field = 0; mutable_field = 0 } in
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial" but is expected to be "total".
|}]

(* Pure discarded expressions and exception handling remain total. *)
type irec = { i : int }
let pure_sequence @ total = fun () -> (); 0
let pure_discard @ total = fun () -> let _ = 0 in 0
let immutable_record_discard @ total =
  fun () -> let _ = { i = 0 } in 0
let try_sequence @ total = fun () -> (try () with _ -> ()); 0
[%%expect{|
type irec = { i : int; }
val pure_sequence : unit -> int = <fun>
val pure_discard : unit -> int = <fun>
val immutable_record_discard : unit -> int = <fun>
val try_sequence : unit -> int = <fun>
|}]

(* try and local exceptions are allowed (not partial operations), so these are
   accepted even in the same let-in-then-force-total position. *)
let _ = let good () = try 1 with _ -> 2 in expects_total good
[%%expect{|
Line 1, characters 43-61:
1 | let _ = let good () = try 1 with _ -> 2 in expects_total good
                                               ^^^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

- : unit -> int = <fun>
|}]

let _ = let good () = let exception E in 0 in expects_total good
[%%expect{|
Line 1, characters 46-64:
1 | let _ = let good () = let exception E in 0 in expects_total good
                                                  ^^^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

- : unit -> int = <fun>
|}]

(* These Lock examples also reject through the ordinary structure-boundary
   default before the following physical-use checks are reached. *)
let late_return_captured_ref () = captured_ref
let _ = expects_total late_return_captured_ref
let _ = expects_physical_ref (late_return_captured_ref ())
[%%expect{|
val late_return_captured_ref : unit -> int ref = <fun>
Line 2, characters 22-46:
2 | let _ = expects_total late_return_captured_ref
                          ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

let late_return_passed_ref (r : int ref) = r
let _ = expects_total late_return_passed_ref
let _ = expects_physical_ref (late_return_passed_ref (ref 0))
[%%expect{|
val late_return_passed_ref : int ref -> int ref = <fun>
Line 2, characters 22-44:
2 | let _ = expects_total late_return_passed_ref
                          ^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "partial" but is expected to be "total".
|}]

(* Parameters are NOT locked (spec-bug fix, ruled 2026-07-16): mirroring
   portability/contention, a total closure's parameters are unconstrained and
   viewed at physical, so these functions print with plain parameter and
   return types (no [@ total] argument, no [@ logical] return). *)
exception Total_try
let (total_apply @ total) g = g 0
let (total_identity @ total) x = x
let (total_project @ total) (x, _) = x
let total_apply_it @ total = fun g -> g 0
let (total_try @ total) x = try x with Total_try -> x
[%%expect{|
exception Total_try
val total_apply : (int -> 'a) -> 'a = <fun>
val total_identity : 'a -> 'a = <fun>
val total_project : 'a * 'b -> 'a = <fun>
val total_apply_it : (int -> 'a) -> 'a = <fun>
val total_try : 'a -> 'a = <fun>
|}]

(* Hereditary: nested function literals in total bodies must be total. *)
let _ @ total =
  fun () ->
    let local @ partial = fun x -> x in
    local
[%%expect{|
Line 3, characters 26-36:
3 |     let local @ partial = fun x -> x in
                              ^^^^^^^^^^
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

(* All comparison primitives are partial, independently of operand type or
   whether the primitive is used directly or through an alias. *)
let _ @ total = fun (x : string) -> x < x
[%%expect{|
Line 1, characters 38-39:
1 | let _ @ total = fun (x : string) -> x < x
                                          ^
Error: The value "(<)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-41
         which is expected to be "total".
|}]

let _ @ total = fun (xs : int list) -> xs = xs
[%%expect{|
Line 1, characters 42-43:
1 | let _ @ total = fun (xs : int list) -> xs = xs
                                              ^
Error: The value "(=)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-46
         which is expected to be "total".
|}]

let _ @ total = fun (x : int) -> x == x
[%%expect{|
Line 1, characters 35-37:
1 | let _ @ total = fun (x : int) -> x == x
                                       ^^
Error: The value "(==)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-39
         which is expected to be "total".
|}]

let _ @ total = fun (x : int) -> x < x
[%%expect{|
Line 1, characters 35-36:
1 | let _ @ total = fun (x : int) -> x < x
                                       ^
Error: The value "(<)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 16-38
         which is expected to be "total".
|}]

let physical_equal = ( == )
let _ @ total = fun (x : int) -> physical_equal x x
[%%expect{|
val physical_equal : 'a -> 'a -> bool = <fun>
Line 2, characters 33-47:
2 | let _ @ total = fun (x : int) -> physical_equal x x
                                     ^^^^^^^^^^^^^^
Error: The value "physical_equal" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 16-51
         which is expected to be "total".
|}]

let _ @ total = fun () -> while false do () done
[%%expect{|
Line 1, characters 26-48:
1 | let _ @ total = fun () -> while false do () done
                              ^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial" but is expected to be "total".
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
