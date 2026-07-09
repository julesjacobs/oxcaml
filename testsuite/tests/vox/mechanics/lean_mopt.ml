type 'v mopt = MMiss | MFound of 'v
let found (x : int) : int mopt{ is_found _ } = MFound x
