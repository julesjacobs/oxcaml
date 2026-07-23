let fact_from_normal_path_must_not_reach_handler (x : int) : int{ _ > 0 } =
  try
    if x > 0 then
      let body_result = x in
      body_result
    else raise Not_found
  with
  | Not_found ->
    let handler_result = x in
    handler_result
