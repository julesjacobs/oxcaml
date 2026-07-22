external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

let[@vox.def] rec member (query : int) (values : int list @ logical) =
  match values with
  | [] -> false
  | value :: rest ->
    if int_equal query value then true else member query rest

let[@vox.def] rec insert (inserted : int)
    (values : int list @ logical) =
  match values with
  | [] -> [ inserted ]
  | value :: rest ->
    if int_equal inserted value
    then values
    else if int_less inserted value
    then inserted :: values
    else value :: insert inserted rest

let[@vox.def] rec all_greater (lower : int)
    (values : int list @ logical) =
  match values with
  | [] -> true
  | value :: rest ->
    if int_less lower value then all_greater lower rest else false

let[@vox.def] rec sorted_unique (values : int list @ logical) =
  match values with
  | [] -> true
  | value :: rest ->
    if all_greater value rest then sorted_unique rest else false

let empty_member_law ~(query : int)
    : unit{ member query [] = false } =
  let _definition = member_def query [] in
  ()

let member_cons_law ~(query : int) ~(value : int)
    ~(rest : int list @ logical)
    : unit{
      member query (value :: rest)
      = ((query = value) || member query rest)
    } =
  let _definition = member_def query (value :: rest) in
  if int_equal query value then () else ()

let finish_insert_member (inserted : int)
    (values : int list @ logical) (query : int)
    (_proof : unit{
      member query (insert inserted values)
      = ((query = inserted) || member query values)
    })
    : unit{
      member query (insert inserted values)
      = ((query = inserted) || member query values)
    } =
  ()

let rec insert_member_law ~(inserted : int)
    ~(values : int list @ logical) ~(query : int)
    : unit{
      member query (insert inserted values)
      = ((query = inserted) || member query values)
    } =
  match values with
  | [] ->
    let _insert = insert_def inserted [] in
    let _result = member_def query [ inserted ] in
    let _empty = member_def query [] in
    if int_equal query inserted
    then finish_insert_member inserted values query ()
    else finish_insert_member inserted values query ()
  | value :: rest ->
    let _old_member = member_def query (value :: rest) in
    let _insert = insert_def inserted (value :: rest) in
    let same = int_equal inserted value in
    match same with
    | true -> finish_insert_member inserted values query ()
    | false ->
      let before = int_less inserted value in
      match before with
      | true ->
        let _result = member_def query (inserted :: value :: rest) in
        if int_equal query inserted
        then finish_insert_member inserted values query ()
        else finish_insert_member inserted values query ()
      | false ->
        insert_member_law ~inserted ~values:rest ~query;
        let _result = member_def query (value :: insert inserted rest) in
        if int_equal query value
        then finish_insert_member inserted values query ()
        else finish_insert_member inserted values query ()
