let bad () =
  let (tx, _ : {n : int | n = 42} One_shot.send *
      {n : int | n = 42} One_shot.recv) = One_shot.create () in
  One_shot.send tx 0
