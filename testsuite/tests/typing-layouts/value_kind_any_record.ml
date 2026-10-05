(* TEST
 flags = "-extension layouts_alpha -extension small_numbers -dlambda -dno-unique-ids";
 expect;
*)

type ('a : any) t = { x : 'a; y : int }
[%%expect{|
0
type ('a : any) t = { x : 'a; y : int; }
|}]

let ints (r : int t) = r
[%%expect{|
(let
  (ints =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : (consts ()) (non_consts ([0: value<int>, value<int>])) r))
  (apply (field_imm 1 (global Toploop!)) "ints" ints))
val ints : int t -> int t = <fun>
|}]

let floats (r : float t) = r
[%%expect{|
(let
  (floats =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: value<float>, value<int>]))>]
       : (consts ()) (non_consts ([0: value<float>, value<int>])) r))
  (apply (field_imm 1 (global Toploop!)) "floats" floats))
val floats : float t -> float t = <fun>
|}]

let unboxed_floats (r : float# t) = r
[%%expect{|
(let
  (unboxed_floats =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: float64, value<int>]))>]
       : (consts ()) (non_consts ([0: float64, value<int>])) r))
  (apply (field_imm 1 (global Toploop!)) "unboxed_floats" unboxed_floats))
val unboxed_floats : float# t -> float# t = <fun>
|}]

let products (r : #(int * float#) t) = r
[%%expect{|
(let
  (products =
     (function {nlocal = 0}
       r[value<
          (consts ())
           (non_consts ([0: product value<int>, float64, value<int>]))>]
       : (consts ())
          (non_consts ([0: product value<int>, float64, value<int>]))
       r))
  (apply (field_imm 1 (global Toploop!)) "products" products))
val products : #(int * float#) t -> #(int * float#) t = <fun>
|}]

let voids (r : unit# t) = r
[%%expect{|
(let
  (voids =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: product , value<int>]))>]
       : (consts ()) (non_consts ([0: product , value<int>])) r))
  (apply (field_imm 1 (global Toploop!)) "voids" voids))
val voids : unit# t -> unit# t = <fun>
|}]

let opaque (type a : any) (r : a t) = r
[%%expect{|
(let
  (opaque =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: ?]))>]
       : (consts ()) (non_consts ([0: ?])) r))
  (apply (field_imm 1 (global Toploop!)) "opaque" opaque))
val opaque : ('a : any). 'a t -> 'a t = <fun>
|}]

type ('a : any) single = { field : 'a }
[%%expect{|
0
type ('a : any) single = { field : 'a; }
|}]

(* Refining [any] to [float] does not produce a flat float record. *)
let boxed_floats (r : float single) = r
[%%expect{|
(let
  (boxed_floats =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: value<float>]))>]
       : (consts ()) (non_consts ([0: value<float>])) r))
  (apply (field_imm 1 (global Toploop!)) "boxed_floats" boxed_floats))
val boxed_floats : float single -> float single = <fun>
|}]

let all_void (r : unit# single) = r
[%%expect{|
(let
  (all_void =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: product ]))>]
       : (consts ()) (non_consts ([0: product ])) r))
  (apply (field_imm 1 (global Toploop!)) "all_void" all_void))
val all_void : unit# single -> unit# single = <fun>
|}]

type ('a : any) mutable_record = { mutable field : 'a }
[%%expect{|
0
type ('a : any) mutable_record = { mutable field : 'a; }
|}]

let mutable_record (r : float# mutable_record) = r
[%%expect{|
(let (mutable_record = (function {nlocal = 0} r r))
  (apply (field_imm 1 (global Toploop!)) "mutable_record" mutable_record))
val mutable_record : float# mutable_record -> float# mutable_record = <fun>
|}]

type ('a : any) inline = A | B of { x : 'a; y : int }
[%%expect{|
0
type ('a : any) inline = A | B of { x : 'a; y : int; }
|}]

(* This match makes it so [r]'s lambda expression isn't equivalent to either
   [v] or [w]'s, so we can see its value kind in the simplified lambda. *)
let either (v : float# inline) (w : float# inline) =
  match v, w with
  | B r, _ | _, B r -> r.y
  | A, A -> 0
[%%expect{|
(let
  (either =
     (function {nlocal = 0}
       v[value<(consts (0)) (non_consts ([0: float64, value<int>]))>]
       w[value<(consts (0)) (non_consts ([0: float64, value<int>]))>] : int
       (catch (if v (exit 11 v) (if w (exit 11 w) 0))
        with (11 r[value<(consts ()) (non_consts ([0: float64, value<int>]))>])
         (mixedfield 1  (float64,value<int>) r))))
  (apply (field_imm 1 (global Toploop!)) "either" either))
val either : float# inline -> float# inline -> int = <fun>
|}]

let primitive_fields
    (r : (int * char * int8 * int16 * bool * unit * string * bytes *
          floatarray * float * float32 * int32 * int64 * nativeint) t) = r
[%%expect{|
(let
  (primitive_fields =
     (function {nlocal = 0}
       r[value<
          (consts ())
           (non_consts ([0:
                         value<
                          (consts ())
                           (non_consts ([0: value<int>, value<int>,
                                         value<int>, value<int>, value<int>,
                                         value<int>, *, *, value<floatarray>,
                                         value<float>, value<float32>,
                                         value<int32>, value<int64>,
                                         value<nativeint>]))>, value<int>]))>]
       : (consts ())
          (non_consts ([0:
                        value<
                         (consts ())
                          (non_consts ([0: value<int>, value<int>,
                                        value<int>, value<int>, value<int>,
                                        value<int>, *, *, value<floatarray>,
                                        value<float>, value<float32>,
                                        value<int32>, value<int64>,
                                        value<nativeint>]))>, value<int>]))
       r))
  (apply (field_imm 1 (global Toploop!)) "primitive_fields" primitive_fields))
val primitive_fields :
  (int * char * int8 * int16 * bool * unit * string * bytes * floatarray *
   float * float32 * int32 * int64 * nativeint)
  t ->
  (int * char * int8 * int16 * bool * unit * string * bytes * floatarray *
   float * float32 * int32 * int64 * nativeint)
  t = <fun>
|}]

module Shadowed = struct
  type int = float
  let float_field (r : int t) = r
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Shadowed/399"
  (let
    (float_field =
       (function {nlocal = 0}
         r[value<(consts ()) (non_consts ([0: value<float>, value<int>]))>]
         : (consts ()) (non_consts ([0: value<float>, value<int>])) r))
    (makeblock 0 float_field)))
module Shadowed : sig type int = float val float_field : int t -> int t end
|}]

let default_sort () =
  match assert false with
  | _ -> assert false
[%%expect{|
(let
  (default_sort =
     (function {nlocal = 0} param[value<int>]
       (let
         (*match* =?
            (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 2 8])))
         (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 3 9])))))
  (apply (field_imm 1 (global Toploop!)) "default_sort" default_sort))
val default_sort : unit -> 'a = <fun>
|}]
