module D = Hm_declarative
module F = Fast_environment

type index = {number : int; original : D.index @@ ghost}
type term = Bound of index | Truth | Lambda of term | Recursive of term
  | Apply of term * term | Let of term * term [@@inductive]
let[@def] rec (source @ total) (term : term @ immutable) = ghost_ (
  match term with Bound i -> D.Bound i.original | Truth -> D.Truth
  | Lambda b -> D.Lambda (source b) | Recursive b -> D.Recursive (source b)
  | Apply (a, b) -> D.Apply (source a, source b)
  | Let (a, b) -> D.Let (source a, source b))
let[@def] rec (valid @ total) (term : term @ immutable) = ghost_ (
  match term with Bound i -> i.number >= 0 && F.encoded i.original i.number
  | Truth -> true | Lambda b | Recursive b -> valid b
  | Apply (a, b) | Let (a, b) -> valid a && valid b)

let rec encode_work : (goal : D.index Ghost.t) @ immutable ->
    (index : D.index) @ immutable ->
    (use : ((n : {n : int | n >= 0 && F.encoded index n}) ->
      {n : int | n >= 0 && F.encoded goal.Ghost.ghost n})) ->
    {n : int | n >= 0 && F.encoded goal.Ghost.ghost n} = fun goal index use ->
  match index with
  | D.Z -> ghost_ (F.encoded_def index 0); let zero = 0 in use (zero)
  | D.S rest ->
    let resume : (n : {n : int | n >= 0 && F.encoded rest n}) ->
        {n : int | n >= 0 && F.encoded goal.Ghost.ghost n} = fun n ->
      let next = n + 1 in
      if next < 0 then failwith "variable index capacity" else (
        ghost_ (F.encoded_def index next);
        use (next)) in
    encode_work goal rest resume

let encode : (index : D.index) @ immutable ->
    {out : index | out.number >= 0 && F.encoded index out.number && out.original === index} @ immutable =
  fun index ->
    let goal = {Ghost.ghost = ghost_ index} in
    let use : (n : {n : int | n >= 0 && F.encoded index n}) ->
        {n : int | n >= 0 && F.encoded goal.Ghost.ghost n} = fun n ->
      n in
    let number = encode_work goal index use in
    let out = {number; original = index} in out

let rec compile_work : (goal : D.term Ghost.t) @ immutable ->
    (term : D.term) @ immutable ->
    (use : ((t : {t : term | valid t && source t === term}) @ immutable ->
      {t : term | valid t && source t === goal.Ghost.ghost} @ immutable)) ->
    {t : term | valid t && source t === goal.Ghost.ghost} @ immutable = fun goal term use ->
  match term with
  | D.Bound i ->
    let index = encode i in let out = Bound index in
    ghost_ (valid_def out; source_def out); use (out)
  | D.Truth -> let out = Truth in
    ghost_ (valid_def out; source_def out); use (out)
  | D.Lambda b ->
    let resume : (body : {t : term | valid t && source t === b}) @ immutable ->
        {t : term | valid t && source t === goal.Ghost.ghost} @ immutable = fun body ->
      let out = Lambda body in
      ghost_ (valid_def out; source_def out); use (out) in
    compile_work goal b resume
  | D.Recursive b ->
    let resume : (body : {t : term | valid t && source t === b}) @ immutable ->
        {t : term | valid t && source t === goal.Ghost.ghost} @ immutable = fun body ->
      let out = Recursive body in
      ghost_ (valid_def out; source_def out); use (out) in
    compile_work goal b resume
  | D.Apply (a, b) ->
    let left : (left : {t : term | valid t && source t === a}) @ immutable ->
        {t : term | valid t && source t === goal.Ghost.ghost} @ immutable = fun left ->
      let right : (right : {t : term | valid t && source t === b}) @ immutable ->
          {t : term | valid t && source t === goal.Ghost.ghost} @ immutable = fun right ->
        let out = Apply (left, right) in
        ghost_ (valid_def out; source_def out); use (out) in
      compile_work goal b right in
    compile_work goal a left
  | D.Let (a, b) ->
    let left : (left : {t : term | valid t && source t === a}) @ immutable ->
        {t : term | valid t && source t === goal.Ghost.ghost} @ immutable = fun left ->
      let right : (right : {t : term | valid t && source t === b}) @ immutable ->
          {t : term | valid t && source t === goal.Ghost.ghost} @ immutable = fun right ->
        let out = Let (left, right) in
        ghost_ (valid_def out; source_def out); use (out) in
      compile_work goal b right in
    compile_work goal a left

let compile : (term : D.term) @ immutable ->
    {t : term | valid t && source t === term} @ immutable = fun term ->
  let goal = {Ghost.ghost = ghost_ term} in
  let use : (t : {t : term | valid t && source t === term}) @ immutable ->
      {t : term | valid t && source t === goal.Ghost.ghost} @ immutable = fun t ->
    t in
  let out = compile_work goal term use in out

let[@def] rec (decode @ total) (n : int) = ghost_ (
  if n <= 0 then D.Z else D.S (decode (n - 1)))
[@@decreases n]

let rec (decode_encoded @ total) : (n : int) ->
    {u : unit | n < 0 || F.encoded (decode n) n} @ ghost = fun n -> ghost_ (
  decode_def n; let index = decode n in F.encoded_def index n;
  (if n > 0 then (let previous = n - 1 in decode_encoded previous; ()) else ());
  ())
[@@decreases n]

let bound : (n : {n : int | n >= 0}) ->
    {t : term | valid t && source t === D.Bound (decode n)} @ immutable =
  fun n -> let index = {number = n; original = decode n} in let out = Bound index in
    ghost_ (decode_encoded n; valid_def out; source_def out);
    out
