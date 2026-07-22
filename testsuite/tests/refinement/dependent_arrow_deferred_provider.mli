val partial : x:int{ 1 = _ } -> unit
val returning_partial : x:(x : int{ 1 = _ }) -> int{ _ = x }
