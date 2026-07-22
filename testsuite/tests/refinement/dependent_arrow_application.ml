(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
*)

let consume ~(x : int)
    ~(witness : query:int -> unit{ x = x }) =
  ignore witness

let forward ~(x : int)
    ~(witness : query:int -> unit{ x = x }) =
  consume ~x ~witness

module type Set = sig
  type t

  val member : int -> t @ local logical -> bool @@ total

  val equal_backward_law :
    t1:t @ logical ->
    t2:t @ logical ->
    pointwise:
      (query:int -> unit{ member query t1 = member query t2 }) @ total ->
    unit @@ total
end

module Client (Set : Set) = struct
  let use_backward_law
      ~(t1 : Set.t @ logical)
      ~(t2 : Set.t @ logical)
      ~(pointwise :
          (query:int ->
           unit{ Set.member query t1 = Set.member query t2 }) @ total) =
    Set.equal_backward_law ~t1 ~t2 ~pointwise
end
