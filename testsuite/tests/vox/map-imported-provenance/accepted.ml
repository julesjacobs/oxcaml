module M = Provider.Anonymous

let (lookup @ total) (m : int M.t) (key : int) (value : int) :
    {u : unit | M.find_opt key (M.add key value m) === Some value}
    @ ghost = ghost_ ()

let (cardinal @ total) (key : int) (value : int) :
    {u : unit | M.cardinal (M.add key value (M.empty ())) = 1Z}
    @ ghost = ghost_ ()
