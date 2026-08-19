(* TEST
 flags = "-drefinements";
 expect;
*)

(* Vox predicate typing (design-docs/predicate-typing.md): every refinement
   predicate is checked, at the point the type is formed, to be a bool by
   Typecore reentry — the hole [_] bound at the payload type, each
   dependent-arrow binder bound at its completed declared type — and the
   checked result is stored as a typed mirror.

   RED pins today's behaviour: predicates are resolved but not typed, so
   ill-typed predicates are accepted.  GREEN flips them to located errors;
   the expectation diff is the demonstration. *)

(* --- Rejections and acceptance, located ------------------------------ *)

type a = int{ 42 };;
[%%expect{|
type a = int{ 42 }
|}]

type b = int{ _ + "x" };;
[%%expect{|
type b = int{ _ + "x" }
|}]

(* The hole is a string, so the application itself is well-typed; its
   [int] result is what must be rejected against the expected [bool]. *)
type c = string{ String.length _ };;
[%%expect{|
type c = string{ String.length _ }
|}]

type d = int{ _ > 0 };;
[%%expect{|
type d = int{ _ > 0 }
|}]

(* Rollback must retain the concrete payload in the eventual diagnostic. *)
type rollback_diagnostic = (int * int){ _ > "s" };;
[%%expect{|
type rollback_diagnostic = (int * int){ _ > "s" }
|}]

(* --- Holes ------------------------------------------------------------ *)

(* multiple occurrences, each at the payload *)
type h1 = int{ _ > 0 && _ < 10 };;
[%%expect{|
type h1 = int{ (_ > 0) && (_ < 10) }
|}]

(* nested refinement: each hole means the innermost enclosing refinement's
   value — the inner hole is a string, the outer an int *)
type h2 = int{ (("s" : string{ String.length _ > 0 }) = "s") && _ > 0 };;
[%%expect{|
type h2 = int{ (("s" : string{ (String.length _) > 0 }) = "s") && (_ > 0) }
|}]

(* the inner hole is *not* the outer int: [_ > 0] at payload string must
   reject *)
type h3 = int{ (("s" : string{ _ > 0 }) = "s") && _ > 0 };;
[%%expect{|
type h3 = int{ (("s" : string{ _ > 0 }) = "s") && (_ > 0) }
|}]

(* holes under the predicate's own binders *)
type h4 = int{ let y = _ in y > 0 };;
[%%expect{|
type h4 = int{ let y = _ in y > 0 }
|}]

type h5 = int{ match _ with 0 -> true | n -> n > 0 };;
[%%expect{|
type h5 = int{ match _ with | 0 -> true | n -> n > 0 }
|}]

type h6 = int{ (fun b -> b && _ > 0) true };;
[%%expect{|
type h6 = int{ (fun b -> b && (_ > 0)) true }
|}]

(* an ill-typed hole use under a binder *)
type h7 = int{ let y = _ in y && true };;
[%%expect{|
type h7 = int{ let y = _ in y && true }
|}]

(* --- Binders ----------------------------------------------------------- *)

(* bare binder at payload *)
type b1 = n:int{ n > 0 } -> unit;;
[%%expect{|
type b1 = n:int{ n > 0 } -> unit
|}]

(* labelled binder at payload *)
type b2 = ~x:int{ x > 0 } -> unit;;
[%%expect{|
type b2 = ~x:int{ x > 0 } -> unit
|}]

(* an ill-typed use of the binder *)
type b3 = n:int{ n ^ "" = "" } -> unit;;
[%%expect{|
type b3 = n:int{ (n ^ "") = "" } -> unit
|}]

(* refined binder head-strip: [x] is used at the payload of its declared
   refined type *)
type b4 = x:int{ x > 0 } -> int{ _ > x };;
[%%expect{|
type b4 = x:int{ x > 0 } -> int{ _ > x }
|}]

(* binder-in-own-domain: [x] is the whole tuple, not an int — flips to a
   type error when predicates are typed *)
type b5 = x:(int{ x > 0 } * int) -> unit;;
[%%expect{|
type b5 = x:int{ x > 0 } * int -> unit
|}]

(* the same shape used correctly: the domain completes before the
   predicate is typed, so [fst x] projects the tuple *)
type b6 = x:(int{ fst x > 0 } * int) -> unit;;
[%%expect{|
type b6 = x:int{ (fst x) > 0 } * int -> unit
|}]

(* labelled spelling of the own-domain pair *)
type b7 = ~x:(int{ x > 0 } * int) -> unit;;
[%%expect{|
type b7 = ~x:int{ x > 0 } * int -> unit
|}]

type b8 = ~x:(int{ fst x > 0 } * int) -> unit;;
[%%expect{|
type b8 = ~x:int{ (fst x) > 0 } * int -> unit
|}]

(* Stored annotations must not create metadata cycles when more than one
   predicate destructures the same completed binder domain, or when a
   codomain predicate follows an own-domain predicate. *)
type b8_multi = x:(int{ fst x > 0 } * int{ snd x > 0 }) -> unit;;
[%%expect{|
type b8_multi = x:int{ (fst x) > 0 } * int{ (snd x) > 0 } -> unit
|}]

type b8_codomain = x:(int{ fst x > 0 } * int) -> bool{ snd x = 0 };;
[%%expect{|
type b8_codomain = x:int{ (fst x) > 0 } * int -> bool{ (snd x) = 0 }
|}]

(* A hole at a dependent-arrow payload must remain equal to its own copied
   type; binder freshening may not desynchronize the nested predicate. *)
type dependent_hole = (x:int -> int{ _ >= x }){ _ = _ };;
[%%expect{|
type dependent_hole = (x:int -> int{ _ >= x }){ _ = _ }
|}]

module type Dependent_hole_signature = sig
  type t = (x:int -> int{ _ >= x }){ _ = _ }
end;;
[%%expect{|
module type Dependent_hole_signature =
  sig type t = (x:int -> int{ _ >= x }){ _ = _ } end
|}]

module Dependent_hole_source = struct
  type t = (x:int -> int{ _ >= x }){ _ = _ }
end;;
[%%expect{|
module Dependent_hole_source :
  sig type t = (x:int -> int{ _ >= x }){ _ = _ } end
|}]

module Dependent_hole_copy (X : Dependent_hole_signature) = X;;
[%%expect{|
module Dependent_hole_copy :
  functor (X : Dependent_hole_signature) ->
    sig type t = (x:int -> int{ _ >= x }){ _ = _ } end
|}]

module Dependent_hole_result : Dependent_hole_signature =
  Dependent_hole_copy (Dependent_hole_source);;
[%%expect{|
module Dependent_hole_result : Dependent_hole_signature
|}]

(* Derived node annotations are metadata and must not add variance
   occurrences to the written declaration. *)
type -'a contravariant = x:'a -> int{ x = x };;
[%%expect{|
type 'a contravariant = x:'a -> int{ x = x }
|}]

(* Predicate typing must not rewrite the written abbreviation spelling in a
   refined binder payload. *)
type refined_string_binder =
  s:string{ String.length s > 0 } -> unit;;
[%%expect{|
type refined_string_binder = s:string{ (String.length s) > 0 } -> unit
|}]

(* predicate binders shadow arrow binders *)
type b9 = n:int -> bool{ (let n = true in n) && n > 0 };;
[%%expect{|
type b9 = n:int -> bool{ (let n = true in n) && (n > 0) }
|}]

(* Binder order-sensitivity: the early annotation constrains [g] before use
   and accepts; the late annotation follows a use that fixes [g] to a bare
   result and must reject the refined result type cleanly. *)
type b10 = bool{
  let f = fun g ->
    let h = (g : unit -> int{ true }) in
    h () + 1 > 0
  in
  true
};;
[%%expect{|
type b10 =
    bool{ let f g = let h = (g : unit -> int{ true }) in ((h ()) + 1) > 0 in
          true }
|}]

type b11 = bool{
  let f = fun g ->
    let y = g () in
    let z = y + 1 in
    let h = (g : unit -> int{ true }) in
    z > 0
  in
  true
};;
[%%expect{|
type b11 =
    bool{ let f g =
            let y = g () in
            let z = y + 1 in let h = (g : unit -> int{ true }) in z > 0 in
          true }
|}]

(* --- Disambiguation ---------------------------------------------------- *)

(* Two records sharing a label name: the parsetree-era resolver froze the
   first candidate; Typecore selects by the expected (payload) type. *)
type r1 = { f : int };;
type r2 = { f : bool };;
[%%expect{|
type r1 = { f : int; }
type r2 = { f : bool; }
|}]

(* [_ : r1], so [_.f] must be [r1]'s int-valued field, not [r2]'s *)
type dr = r1{ _.f > 0 };;
[%%expect{|
type dr = r1{ _.f > 0 }
|}]

(* and [r2]'s field is a bool *)
type dr2 = r2{ _.f };;
[%%expect{|
type dr2 = r2{ _.f }
|}]

(* Queued predicates must observe only authoritative Typecore identities in
   other refinements of the same completed domain, in either source order. *)
module Queue_int = struct type t = { shared : int } end;;
module Queue_string = struct type t = { shared : string } end;;
open Queue_int;;
open Queue_string;;
[%%expect{|
module Queue_int : sig type t = { shared : int; } end
module Queue_string : sig type t = { shared : string; } end
|}]

type queue_wanted = Queue_int.t{ _.shared > 0 };;
let accepts_queue_wanted (_ : queue_wanted list) = true;;
[%%expect{|
type queue_wanted = Queue_int.t{ _.shared > 0 }
val accepts_queue_wanted : queue_wanted list -> bool = <fun>
|}]

type queue_observes_later =
  x:(unit{ accepts_queue_wanted (snd x) }
     * Queue_int.t{ _.shared > 0 } list) -> unit;;
[%%expect{|
type queue_observes_later =
    x:unit{ accepts_queue_wanted (snd x) } * Queue_int.t{ _.shared > 0 } list ->
    unit
|}]

type queue_observes_earlier =
  x:(Queue_int.t{ _.shared > 0 } list
     * unit{ accepts_queue_wanted (fst x) }) -> unit;;
[%%expect{|
type queue_observes_earlier =
    x:Queue_int.t{ _.shared > 0 } list * unit{ accepts_queue_wanted (fst x) } ->
    unit
|}]

(* the selection is observable: [r1]'s [f] is an int, so using it as a
   bool is an error naming int — a first-candidate resolver would have
   picked [r2]'s bool-valued [f] and accepted *)
type drx = r1{ _.f && true };;
[%%expect{|
type drx = r1{ _.f && true }
|}]

(* Two variants sharing a constructor name, selected by expected type *)
type v1 = A of int;;
type v2 = A of bool;;
[%%expect{|
type v1 = A of int
type v2 = A of bool
|}]

type dv = v1{ _ = A 1 };;
[%%expect{|
type dv = v1{ _ = (A 1) }
|}]

type dv2 = v1{ match _ with A n -> n > 0 };;
[%%expect{|
type dv2 = v1{ match _ with | A n -> n > 0 }
|}]

(* the pattern selection is observable too: [v1]'s [A] carries an int *)
type dvx = v1{ match _ with A n -> n && true };;
[%%expect{|
type dvx = v1{ match _ with | A n -> n && true }
|}]

(* --- Application -------------------------------------------------------- *)

module App = struct
  let labelled ~x ~y = x > 0 && y
  let optional ?(o = 0) () = o > 0
  let positional ~(p : [%call_pos]) () = p.Lexing.pos_lnum > 0
end;;
[%%expect{|
module App :
  sig
    val labelled : x:int -> y:bool -> bool
    val optional : ?o:int -> unit -> bool
    val positional : p:[%call_pos] -> unit -> bool
  end
|}]

(* labelled application with commuting *)
type ap1 = int{ App.labelled ~y:true ~x:_ };;
[%%expect{|
type ap1 = int{ App.labelled ~y:true ~x:_ }
|}]

(* partial application of a labelled function *)
type ap2 = int{ (App.labelled ~y:true) ~x:_ };;
[%%expect{|
type ap2 = int{ (App.labelled ~y:true) ~x:_ }
|}]

(* Optional/Position arrows in an applied callee's type: rejected in all
   four spellings *)
type ap3 = int{ App.optional ~o:_ () };;
[%%expect{|
type ap3 = int{ App.optional ~o:_ () }
|}]

type ap4 = int{ App.optional ?o:(Some _) () };;
[%%expect{|
type ap4 = int{ App.optional ?o:(Some _) () }
|}]

type ap5 = int{ App.optional () };;
[%%expect{|
type ap5 = int{ App.optional () }
|}]

type ap6 = int{ App.positional () };;
[%%expect{|
type ap6 = int{ App.positional () }
|}]

(* The clean spelling: the omission is well-typed on its own, so the located
   rejection is the mirror's. *)
type ap7b = bool{ let g = App.labelled ~y:true in g ~x:1 };;
[%%expect{|
type ap7b = bool{ let g = App.labelled ~y:true in g ~x:1 }
|}]

(* %apply / %revapply rewrites have no faithful preimage *)
type ap8 = int{ (fun n -> n > 0) @@ _ };;
[%%expect{|
type ap8 = int{ (fun n -> n > 0) @@ _ }
|}]

type ap9 = int{ _ |> fun n -> n > 0 };;
[%%expect{|
type ap9 = int{ _ |> (fun n -> n > 0) }
|}]

(* An inferred RHS is genuinely rewritten as [%revapply], unlike the lambda
   control above, and therefore has no faithful mirror preimage. *)
type ap9b = bool{ true |> Fun.id };;
[%%expect{|
type ap9b = bool{ true |> Fun.id }
|}]

(* a format-string rewrite has no faithful preimage *)
type ap10 = int{ (Printf.sprintf "%d" _) = "0" };;
[%%expect{|
type ap10 = int{ (Printf.sprintf "%d" _) = "0" }
|}]

(* --- Polymorphic let inside a predicate --------------------------------- *)

type pl = int{ let id = fun x -> x in id 0 = 0 && id true };;
[%%expect{|
type pl = int{ let id x = x in ((id 0) = 0) && (id true) }
|}]

(* A simple binding annotation has a faithful [Rexp_constraint] preimage and
   must not become an unsupported-form flip. *)
type pla = int{ let y : int = _ in y > 0 };;
[%%expect{|
type pla = int{ let y = _ in y > 0 }
|}]

(* --- Occurrence strips inside predicates --------------------------------- *)

module Occ = struct
  let g : unit -> int{ _ > 0 } = fun () -> 1
  let l : int{ _ > 0 } list = [1]
end;;
[%%expect{|
Line 2, characters 43-44: refinement obligation: int{ _ > 0 }
Line 3, characters 31-32: refinement obligation: int{ _ > 0 }
module Occ : sig val g : unit -> int{ _ > 0 } val l : int{ _ > 0 } list end
|}]

(* application-result head strip: [Occ.g ()] is an int inside a predicate *)
type oc1 = bool{ Occ.g () + 1 > 0 };;
[%%expect{|
type oc1 = bool{ ((Occ.g ()) + 1) > 0 }
|}]

(* nested heads stay intact: int{p} list is not int list *)
type oc2 = bool{ Occ.l = [1] };;
[%%expect{|
type oc2 = bool{ Occ.l = [1] }
|}]

let accepts_plain_int_list (_ : int list) = true;;
[%%expect{|
val accepts_plain_int_list : int list -> bool = <fun>
|}]

(* This call is rejected only if the nested element refinement remains
   distinct; polymorphic equality in [oc2] alone cannot discriminate that. *)
type oc2b = bool{ accepts_plain_int_list Occ.l };;
[%%expect{|
type oc2b = bool{ accepts_plain_int_list Occ.l }
|}]

(* element projection strips the element's head *)
type oc3 = bool{ List.hd Occ.l > 0 };;
[%%expect{|
type oc3 = bool{ (List.hd Occ.l) > 0 }
|}]

(* a well-typed call to a partial function is accepted by this piece
   (modes are a later piece) *)
type oc4 = bool{ List.hd [] };;
[%%expect{|
type oc4 = bool{ List.hd [] }
|}]

(* a qualified mutable access is likewise accepted by this piece *)
module Mut = struct let cell = { contents = 0 } end;;
type oc5 = bool{ Mut.cell.contents = 0 };;
[%%expect{|
module Mut : sig val cell : int ref end
type oc5 = bool{ Mut.cell.contents = 0 }
|}]

(* --- Refined interior constraint: payload-checked, no obligation -------- *)

(* The refined constraint checks [5] against the payload; the obligation
   Typecore records inside the predicate is discarded with the frame, so
   the probe prints no "refinement obligation" line for it. *)
type ric = int{ (5 : int{ _ > 0 }) = _ };;
[%%expect{|
type ric = int{ (5 : int{ _ > 0 }) = _ }
|}]

(* an interior constraint failing against its payload *)
type ric2 = int{ ("s" : int{ _ > 0 }) = _ };;
[%%expect{|
type ric2 = int{ ("s" : int{ _ > 0 }) = _ }
|}]

(* --- Ambient type variables ---------------------------------------------- *)

(* ['a{ _ = 0 }] pins ['a = int] by ordinary inference *)
let pin : 'a -> 'a{ _ = 0 } = fun x -> x;;
[%%expect{|
Line 1, characters 39-40: refinement obligation: 'a{ _ = 0 }
val pin : 'a -> 'a{ _ = 0 } = <fun>
|}]

(* a predicate may not introduce a new named type variable — declaration
   context (Closed policy)... *)
type nv = int{ (1 : 'newvar) = 1 };;
[%%expect{|
Line 1, characters 20-27:
1 | type nv = int{ (1 : 'newvar) = 1 };;
                        ^^^^^^^
Error: The type variable "'newvar" is unbound in this type declaration.
|}]

(* ...and expression-annotation context (Open policy) *)
let nv2 (x : int{ (1 : 'newvar2) = 1 }) = x;;
[%%expect{|
val nv2 : ('newvar2 : any). int{ (1 : 'newvar2) = 1 } -> int = <fun>
|}]

(* the enclosing declaration's named variables remain visible, and an
   interior constraint type must not clear the enclosing bookkeeping:
   both occurrences of ['a] below are the same variable *)
let tv : 'a -> int{ (0 : int) = _ } -> 'a = fun x _ -> x;;
[%%expect{|
val tv : 'a -> int{ (0 : int) = _ } -> 'a = <fun>
|}]

(* --- Recursive groups ------------------------------------------------------ *)

(* a predicate mentioning a constructor of its own group cannot resolve it *)
type t = RC of int
and u = int{ (RC _ : t) = RC 0 };;
[%%expect{|
Line 2, characters 14-16:
2 | and u = int{ (RC _ : t) = RC 0 };;
                  ^^
Error: Unbound constructor "RC"
|}]

(* likewise a field of its own group *)
type r = { g : int }
and w = r{ _.g > 0 };;
[%%expect{|
Line 2, characters 13-14:
2 | and w = r{ _.g > 0 };;
                 ^
Error: Unbound record field "g"
|}]

(* the same restriction in a signature *)
module type SRec = sig
  type t = RD of int
  and u = int{ (RD _ : t) = RD 0 }
end;;
[%%expect{|
Line 3, characters 16-18:
3 |   and u = int{ (RD _ : t) = RD 0 }
                    ^^
Error: Unbound constructor "RD"
|}]

module type SRecField = sig
  type r = { g : int }
  and w = r{ _.g > 0 }
end;;
[%%expect{|
Line 3, characters 15-16:
3 |   and w = r{ _.g > 0 }
                   ^
Error: Unbound record field "g"
|}]

(* non-group mentions are fine *)
type earlier = B of int;;
type later = int{ (B _ : earlier) = B 0 };;
[%%expect{|
type earlier = B of int
type later = int{ (B _ : earlier) = (B 0) }
|}]

(* --- GADT / existential constructor patterns ------------------------------- *)

type _ g = I : int g;;
[%%expect{|
type _ g = I : int g
|}]

type gd = int{ match (I : int g) with I -> _ > 0 };;
[%%expect{|
type gd = int{ match (I : int g) with | I -> _ > 0 }
|}]

type ex = E : 'a -> ex;;
[%%expect{|
type ex = E : 'a -> ex
|}]

type exd = int{ match E 1 with E _ -> true };;
[%%expect{|
type exd = int{ match E 1 with | E _ -> true }
|}]

(* --- Frame hygiene ------------------------------------------------------ *)

(* a failing predicate inside a signature does not corrupt subsequent
   typing *)
module type Hyg = sig
  val bad : int{ 42 }
  val good : int -> int
end;;
[%%expect{|
module type Hyg = sig val bad : int{ 42 } val good : int -> int end
|}]

let after_hyg = 1 + 1;;
[%%expect{|
val after_hyg : int = 2
|}]

(* rollback: a predicate that unifies against an imported polymorphic
   scheme and then fails must not leave links behind — [List.length] is
   usable afterwards at any instance *)
module type Roll = sig
  val bad2 : int{ List.length _ }
end;;
[%%expect{|
module type Roll = sig val bad2 : int{ List.length _ } end
|}]

let after_roll = List.length [ "a" ] + List.length [ 1 ];;
[%%expect{|
val after_roll : int = 2
|}]

(* commit: a predicate's successful ambient constraints stick — the
   predicate pins the weak variable of [r] to int *)
let commit_r = ref [];;
[%%expect{|
val commit_r : '_weak1 list ref = {contents = []}
|}]

type commit_probe = bool{ commit_r.contents = [ 1 ] };;
[%%expect{|
type commit_probe = bool{ commit_r.contents = [1] }
|}]

let commit_after = commit_r := [ "s" ];;
[%%expect{|
val commit_after : unit = ()
|}]

(* Predicate typing commits the weak variable's arrow shape but rolls back
   predicate-local mode constraints.  The unconstrained arrow modes therefore
   default conservatively, and a later local-argument demand is rejected. *)
let mode_commit_r = ref None;;
[%%expect{|
val mode_commit_r : '_weak2 option ref = {contents = None}
|}]

type mode_commit_probe = bool{
  match mode_commit_r.contents with
  | None -> true
  | Some f -> f 0 = 0
};;
[%%expect{|
type mode_commit_probe =
    bool{ match mode_commit_r.contents with
          | None -> true
          | Some f -> (f 0) = 0 }
|}]

let wants_local_argument (f : local_ int -> int) = f 0;;
[%%expect{|
val wants_local_argument : (int @ local -> int) -> int = <fun>
|}]

let mode_commit_after () =
  match mode_commit_r.contents with
  | None -> 0
  | Some f -> wants_local_argument f;;
[%%expect{|
val mode_commit_after : unit -> int = <fun>
|}]

(* mode boundary: a partial call inside a predicate must not make the
   enclosing closure partial *)
let mode_wall @ total = fun (b : bool{ List.hd [ true ] }) -> b;;
[%%expect{|
val mode_wall : bool{ List.hd [true] } -> bool = <fun>
|}]

(* This piece is type-only: reading a ghost value is admitted and leaves the
   later mode/ghost policy undecided. *)
let uses_ghost_in_predicate (x : int @ ghost) =
  let (_ : int{ x = 0 }) = 0 in
  ();;
[%%expect{|
Line 2, characters 27-28: refinement obligation: int{ x = 0 }
val uses_ghost_in_predicate : int @ ghost -> unit = <fun>
|}]

(* --- Predicate-local binder in a nested refinement, freshened ------------- *)

(* [k] is a predicate-local binder mentioned by a nested refinement's
   interior type; functor application copies the signature with fresh
   binder stamps, and the copy must remain alpha-equal to the original. *)
module type SK = sig
  val v : int{ let k = 1 in (k : int{ _ = k }) = 1 }
end;;
[%%expect{|
module type SK = sig val v : int{ let k = 1 in (k : int{ _ = k }) = 1 } end
|}]

module FK (X : SK) = X;;
[%%expect{|
module FK :
  functor (X : SK) ->
    sig val v : int{ let k = 1 in (k : int{ _ = k }) = 1 } end
|}]

module MK = struct
  let v : int{ let k = 1 in (k : int{ _ = k }) = 1 } = 1
end;;
[%%expect{|
Line 2, characters 6-7: refined environment entry: v :
  int{ let k = 1 in (k : int{ _ = k }) = 1 }
Line 2, characters 55-56: refinement obligation:
  int{ let k = 1 in (k : int{ _ = k }) = 1 }
Line 2, characters 55-56: refinement obligation:
  int{ let k = 1 in (k : int{ _ = k }) = 1 }
module MK : sig val v : int{ let k = 1 in (k : int{ _ = k }) = 1 } end
|}]

module GK : SK = FK (MK);;
[%%expect{|
Line 1, characters 17-24:
1 | module GK : SK = FK (MK);;
                     ^^^^^^^
Error: Modules do not match:
       sig val v : int{ let k = 1 in (k : int{ _ = k }) = 1 } end
     is not included in SK
     Values do not match:
       val v : int{ let k = 1 in (k : int{ _ = k }) = 1 }
     is not included in
       val v : int{ let k = 1 in (k : int{ _ = k }) = 1 }
     The type "int{ let k = 1 in (k : int{ _ = k }) = 1 }"
     is not compatible with the type
       "int{ let k = 1 in (k : int{ _ = k }) = 1 }"
     Type "int{ _ = k }" is not compatible with type "int{ _ = k }"
|}]

(* --- Signature sequencing --------------------------------------------------- *)

(* a signature predicate sees earlier items only *)
module type Seq = sig
  val bound : int
  val v : int{ _ > bound }
end;;
[%%expect{|
Line 1:
Error: Module type declarations do not match:
         module type Seq = sig val bound : int val v : int{ _ > bound } end
       does not match
         module type Seq = sig val bound : int val v : int{ _ > bound } end
       At position "module type Seq = <here>"
       Module types do not match:
         sig val bound : int val v : int{ _ > bound } end
       is not equal to
         sig val bound : int val v : int{ _ > bound } end
       At position "module type Seq = <here>"
       Values do not match:
         val v : int{ _ > bound/2 }
       is not included in
         val v : int{ _ > bound }
       The type "int{ _ > bound }" is not compatible with the type
         "int{ _ > bound/2 }"
       File "_none_", line 1:
         Definition of value "bound/1"
       File "_none_", line 1:
         Definition of value "bound/2"
|}]

module type SeqBad = sig
  val v : int{ _ > bound }
  val bound : int
end;;
[%%expect{|
Line 2, characters 19-24:
2 |   val v : int{ _ > bound }
                       ^^^^^
Error: Unbound value "bound"
|}]
