module Global = struct
  let flag = true
end

module Global_result = Producer.Make (Global)
let retained = Global_result.value

let escaped =
  let module Local = struct
    let flag = true
  end in
  let module M = Producer.Make (Local) in
  M.value
