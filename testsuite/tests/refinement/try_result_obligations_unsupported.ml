let unsupported_return_leaf () : unit{ true } =
  try
    while false do
      ()
    done
  with
  | Exit -> ()
