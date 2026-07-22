(* TEST
 expect;
*)

module Positive : sig end = struct
  module Api : sig
    val relation : int @ logical -> int @ logical -> bool @@ total
    val plus_one : int @ logical -> int @@ total
    val law :
      x:int @ logical ->
      y:int @ logical ->
      unit{ relation x y = true } @@ total
  end = struct
    let[@vox.def] relation
        (_x : int @ logical) (_y : int @ logical) =
      true

    let plus_one (x : int @ logical) = x + 1

    let law ~(x : int @ logical) ~(y : int @ logical)
        : unit{ relation x y = true } =
      let _ = relation_def x y in
      ()
  end

  let partial = Api.law ~x:11
  let () = partial ~y:22
  let partial_fact : bool{ _ = true } = Api.relation 11 22

  let commuted =
    let () = Api.law ~y:22 ~x:11 in
    (Api.relation 11 22 : bool{ _ = true })

  let omitted = Api.law ~y:22
  let () = omitted ~x:11
  let omitted_fact : bool{ _ = true } = Api.relation 11 22

  let compound =
    let () = Api.law ~x:(Api.plus_one 10) ~y:22 in
    (Api.relation (Api.plus_one 10) 22 : bool{ _ = true })

  let consume ~(x : int)
      ~(witness : (q:int -> unit{ q = x }) @ total) =
    ignore witness

  let forward ~(x : int)
      ~(witness : (q:int -> unit{ q = x }) @ total) =
    consume ~x ~witness

  let identity (x : int) : int{ _ = x } = x
  let one = identity 1
  let two = identity 2
end

[%%expect {|
module Positive : sig end
|}]
