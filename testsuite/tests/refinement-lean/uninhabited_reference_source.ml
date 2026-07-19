type void = |
external absurd : int -> void @@ total = "%identity"
let[@vox.def] sink (x : int @ logical) : void = absurd x
let contradiction (x : int @ logical) : int{ _ = 0 && _ = 1 } =
  let _proof = sink_def x in
  x
