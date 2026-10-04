module S = Vox_sequence

let[@def] (insert @ total) (source : int list @ immutable)
    (position : Bigint.t) (value : int) =
  S.append (S.take position source) (value :: S.drop position source)

let[@def] (remove @ total) (source : int list @ immutable)
    (position : Bigint.t) =
  S.append (S.take position source) (S.drop (Bigint.add position 1Z) source)
