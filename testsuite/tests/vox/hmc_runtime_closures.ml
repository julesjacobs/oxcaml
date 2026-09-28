module D = Hm_declarative
module W = Hmc_word64
module K = Hmc_closure_ir
module C = Hmc_cfg_program
module F = Hmc_frame_codec
module Index = Hmc_u32_index

type descriptor = {start : W.limb; captures : W.limb; recursive : bool}
type table = Empty | Add of descriptor * table [@@inductive]
let[@def] rec (size @ total) (table : table @ immutable) = match table with Empty -> D.Z | Add (_, rest) -> D.S (size rest)
let[@def] rec (lookup @ total) (table : table @ immutable) (code : W.limb) = match table with
  | Empty -> None | Add (descriptor, rest) -> if Index.represents (size rest) code then Some descriptor else lookup rest code
let[@def] (describes @ total) (descriptor : descriptor @ immutable) (source : K.entry @ immutable) (compiled : C.function_entry @ immutable) = ghost_ (
  Index.represents compiled.C.start descriptor.start && Index.represents (F.locals_size source.K.captured) descriptor.captures
  && descriptor.recursive = source.K.recursive)
let[@def] rec (related @ total) (source : K.table @ immutable) (functions : C.functions @ immutable) (runtime : table @ immutable) = ghost_ (
  match source, functions, runtime with
  | K.Empty, C.No_functions, Empty -> true
  | K.Add (entry, rest), C.Function (compiled, tail), Add (descriptor, remaining) -> describes descriptor entry compiled && related rest tail remaining
  | _ -> false)
let[@def] rec (encodable @ total) (capacity : W.limb) (source : K.table @ immutable) (functions : C.functions @ immutable) =
  match source, functions with
  | K.Empty, C.No_functions -> true
  | K.Add (entry, rest), C.Function (compiled, tail) -> Index.fits compiled.C.start capacity
      && Index.fits (F.locals_size entry.K.captured) capacity && encodable capacity rest tail
  | _ -> false
let rec (lower @ total) : (capacity : W.limb) -> (source : K.table) @ immutable -> (functions : C.functions) @ immutable ->
    {out : table option | match out with None -> not (encodable capacity source functions)
      | Some runtime -> encodable capacity source functions && related source functions runtime} @ immutable = fun capacity source functions ->
  ghost_ (encodable_def capacity source functions);
  match source, functions with
  | K.Empty, C.No_functions -> ghost_ (related_def source functions Empty); Some Empty
  | K.Add (entry, rest), C.Function (compiled, tail) ->
    (match Index.encode capacity compiled.C.start, Index.encode capacity (F.locals_size entry.K.captured), lower capacity rest tail with
    | Some start, Some captures, Some remaining ->
      let descriptor = {start; captures; recursive = entry.K.recursive} in
      let runtime = Add (descriptor, remaining) in
      ghost_ (describes_def descriptor entry compiled; related_def source functions runtime); Some runtime
    | _ -> None)
  | _ -> None
let rec (same_size @ total) : (source : K.table) @ immutable -> (functions : C.functions) @ immutable -> (runtime : table) @ immutable ->
    {u : unit | related source functions runtime} -> {u : unit | size runtime === K.size source && size runtime === C.size functions} @ ghost =
  fun source functions runtime premise -> ghost_ (
    related_def source functions runtime; size_def runtime; K.size_def source; C.size_def functions;
    match source, functions, runtime with K.Add (_, rest), C.Function (_, tail), Add (_, remaining) -> same_size rest tail remaining () | _ -> ())
let rec (lookup_correct @ total) : (source : K.table) @ immutable -> (functions : C.functions) @ immutable -> (runtime : table) @ immutable ->
    (id : D.index) @ immutable -> (code : W.limb) ->
    {u : unit | related source functions runtime && Index.represents id code} ->
    {u : unit | match lookup runtime code with
      | None -> K.lookup source id === None && C.lookup functions id === None
      | Some descriptor -> (match K.lookup source id, C.lookup functions id with
        | Some entry, Some compiled -> describes descriptor entry compiled | _ -> false)} @ ghost =
  fun source functions runtime id code premise -> ghost_ (
    related_def source functions runtime; lookup_def runtime code; K.lookup_def source id; C.lookup_def functions id;
    match source, functions, runtime with
    | K.Add (_, rest), C.Function (_, tail), Add (_, remaining) ->
      same_size rest tail remaining ();
      let _ = Hm_elaboration_check.index_equal id (K.size rest) in
      let _ = Hm_elaboration_check.index_equal id (C.size tail) in
      if Index.represents (size remaining) code then Index.injective id (size remaining) code ()
      else lookup_correct rest tail remaining id code ()
    | _ -> ())
