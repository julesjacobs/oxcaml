module Make (K : sig type t : immutable_data end) = struct
  external fabricate : int -> K.t @@ total = "%identity"

  let contradiction (value : int @ logical)
      : int{
        fabricate value = fabricate value
        && _ = 0
        && _ = 1
      } =
    value
end
