(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

module Callback = struct
  let run : ('a : immutable_data) ('r : immutable_data).
      (value : 'a) ->
      (post : ('r @ immutable -> 'a @ immutable -> bool @ ghost)) @ ghost ->
      ((input : {input : 'a | input === value}) ->
       {result : 'r | let refine_ input = input in post result input}) ->
      {pair : 'r * 'a | match pair with result, output -> post result output} =
    fun value post body ->
    let input : {input : _ | input === value} = refine_ value in
    let refine_ result = body input in
    let pair = result, value in
    refine_ pair
end;;
[%%expect{|
module Callback :
  sig
    val run :
      ('a : immutable_data) ('r : immutable_data).
        (value : 'a) ->
        (post : ('r @ immutable -> 'a @ immutable -> bool @ ghost)) @ ghost ->
        ((input' : {input : 'a | input === value}) ->
         {result : 'r | let refine_ input = input' in post result input}) ->
        {pair : 'r * 'a
          | match pair with | (result, output) -> post result output}
  end
|}]

module Client = struct
  open Callback
  let client (value : int) : {result : int | result = value + 1} =
    let before = ghost_ value in
    let[@def] (post @ total) (result : int @ immutable)
        (after : int @ immutable) =
      ghost_ (result = after + 1 && after = before)
    in
    let erased_post = ghost_ post in
    let refine_ pair = run value erased_post (fun input ->
      let refine_ input = input in
      let result = input + 1 in
      ghost_ (post_def result input);
      refine_ result)
    in
    let result, output = pair in
    ghost_ (post_def result output);
    refine_ result
end;;
[%%expect{|
module Client :
  sig val client : (value : int) -> {result : int | result = (value + 1)} end
|}]

module Model = struct
  type ('a : immutable_data) t =
    { storage : 'a array;
      contents : 'a list @@ ghost }

  let[@def] (contents @ total) (value @ local immutable) =
    value.contents

  let observe (value @ unique) =
    let _before = ghost_ (contents (borrow_ value)) in
    value
end;;
[%%expect{|
module Model :
  sig
    type ('a : immutable_data) t = {
      storage : 'a array;
      contents : 'a list @@ ghost;
    }
    val contents :
      ('a : immutable_data).
        'a t @ local immutable -> 'a list @ total stateful ghost
    val contents_def :
      ('a : immutable_data).
        (value : 'a t) @ immutable ->
        {u : unit | (contents value) === value.contents}
    val observe : ('a : immutable_data). 'a t @ unique total -> 'a t
  end
|}]
