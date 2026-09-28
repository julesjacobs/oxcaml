module D = Hm_declarative
module W = Hmc_word64
module I = Hmc_u32_index
let rec (correct @ total) : (first : D.index) @ immutable -> (second : D.index) @ immutable ->
    (a : W.limb) -> (b : W.limb) -> (total : W.limb) ->
    {u : unit | I.represents first a && I.represents second b && total = a + b} ->
    {u : unit | I.represents (D.add first second) total} @ ghost = fun first second a b total premise -> ghost_ (
    I.represents_def first a; D.add_def first second; I.represents_def (D.add first second) total;
    match first with D.Z -> () | D.S rest -> correct rest second (a - 1) b (total - 1) ())
