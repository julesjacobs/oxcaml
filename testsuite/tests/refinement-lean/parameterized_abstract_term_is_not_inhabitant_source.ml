module Make (K : sig
  type 'a t : immutable_data
  val int_witness : int t
end) = struct
  let int_witness = K.int_witness
  external bool_result : int -> bool K.t @@ total = "%identity"

  let contradiction (value : int @ logical)
      : int{
        bool_result value = bool_result value
        && _ = 0
        && _ = 1
      } =
    value
end
