type left =
  a:int @ logical ->
  b:int @ logical ->
  c:int{ _ = (a * 10) + b } @ logical ->
  unit{ c = c }

type right =
  a:int @ logical ->
  b:int @ logical ->
  c:int{ _ = (b * 10) + a } @ logical ->
  unit{ c = c }

let swapped_predicate (f : left) : right = (f :> right)
