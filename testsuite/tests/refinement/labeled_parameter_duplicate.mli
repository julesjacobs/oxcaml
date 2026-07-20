module type S = sig
  val law :
    x:int @ logical ->
    x:int{ _ = x + 10 } @ logical ->
    unit{ x >= 10 } @@ total
end
