(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Recursive_callback = struct
  type chain = Stop | Next of chain [@@inductive]
  let rec (proof @ total) : (xs : chain) @ immutable -> (q : int) ->
      {u : unit | true} @ ghost = fun xs q -> ghost_ (
    match xs with
    | Stop -> let u = () in refine_ u
    | Next rest ->
      let callback : (x : int) -> {u : unit | true} @ total =
        fun x -> proof rest x in
      callback q)
end;;
[%%expect{|
module Recursive_callback :
  sig
    type chain = Stop | Next of chain
    [@@inductive]
    val proof : chain @ immutable -> int -> {u : unit | true} @ ghost
  end
|}]

module Pointwise = struct
  type chain = Stop | Next of chain [@@inductive]
  let rec (proof @ total) : (xs : chain) @ immutable -> (q : int) ->
      {u : unit | true} @ ghost = fun xs q -> ghost_ (
    match xs with
    | Stop -> let u = () in refine_ u
    | Next rest -> proof rest q)
end;;
[%%expect{|
module Pointwise :
  sig
    type chain = Stop | Next of chain
    [@@inductive]
    val proof : chain @ immutable -> int -> {u : unit | true} @ ghost
  end
|}]

type direct = #{ value : int; evidence : unit @@ ghost };;
[%%expect{|
type direct = #{ value : int; evidence : unit @@ ghost; }
|}]

type wrapped = #{ value : int; evidence : unit Ghost.t };;
[%%expect{|
type wrapped = #{ value : int; evidence : unit Ghost.t; }
|}]

type top_chain = Stop | Next of top_chain [@@inductive];;
[%%expect{|
type top_chain = Stop | Next of top_chain [@@inductive]
|}]

let rec (top_pointwise @ total) : (xs : top_chain) @ immutable -> (q : int) ->
    {u : unit | true} @ ghost = fun xs q -> ghost_ (
  match xs with
  | Stop -> let u = () in refine_ u
  | Next rest -> top_pointwise rest q);;
[%%expect{|
val top_pointwise : top_chain @ immutable -> int -> {u : unit | true} @ ghost =
  <fun>
|}]
