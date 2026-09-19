type mask = {m : int | 0 <= m && m <= 65535}

external match16_valid : bytes -> int -> int -> mask
  = "caml_vox_control_match16" [@@noalloc]

let[@inline always] match16 bytes offset byte : mask =
  let length = Bytes.length bytes in
  if offset < 0 || length < 16 || offset > length - 16
     || byte < 0 || byte > 255 then invalid_arg "Vox_control.match16";
  refine_ (match16_valid bytes offset byte)

let fingerprint (hash : int) : {b : int | 0 <= b && b <= 127} =
  refine_ (hash land 127)

let clear_first (m : mask) :
    {r : mask | r = (m land (m - 1)) && (m = 0 || r < m)} =
  refine_ (m land (m - 1))
