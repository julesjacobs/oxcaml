module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Plan = Wasm_parallel_copy
module Geometry = Hmc_wasm_relayout_geometry
module Copy = Wasm_cross_copy_prefix
module Lower = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Counts = Hmc_wasm_schema_counts
module Range = Hmc_wasm_range_prefix

let rec (range @ total) : (plan : Plan.plan) @ immutable -> (tail : Plan.plan) @ immutable ->
    (source : int) -> (destination : int) -> (length : D.index) @ immutable ->
    {u : unit | Geometry.split plan source destination length === Some tail} ->
    {u : unit | Lower.range_is plan source destination length tail} @ ghost = fun plan tail source destination length premise -> ghost_ (
      Geometry.split_def plan source destination length; Lower.range_is_def plan source destination length tail;
      match length with
      | D.Z -> ()
      | D.S remaining -> match plan with
        | Plan.Copy (_, _, Plan.Copy (_, _, rest)) -> range rest tail (source + 1) (destination + 1) remaining ()
        | _ -> ())

let (split @ total) : (plan : Plan.plan) @ immutable -> (tail : Plan.plan) @ immutable ->
    (source : int) -> (destination : int) -> (length : D.index) @ immutable -> (capacity : Lower.count) ->
    (base : B.u32) -> (stop : B.u32) ->
    {u : unit | Geometry.split plan source destination length === Some tail && Index.fits length capacity
      && 0 <= destination && destination <= capacity && destination + Geometry.size length <= capacity
      && stop = base + 16 + 16 * capacity && Copy.bounded tail base base stop} ->
    {u : unit | Copy.bounded plan base base stop} @ ghost = fun plan tail source destination length capacity base stop premise -> ghost_ (
      let count = Counts.encode length capacity () in
      range plan tail source destination length ();
      Range.bounded plan tail source destination length count base base stop ())

let (two @ total) : (plan : Plan.plan) @ immutable -> (a : int) -> (b : int) -> (n : D.index) @ immutable ->
    (c : int) -> (d : int) -> (m : D.index) @ immutable -> (base : B.u32) -> (capacity : Lower.count) -> (stop : B.u32) ->
    {u : unit | Geometry.two plan a b n c d m && Index.fits n capacity && Index.fits m capacity
      && 0 <= b && b <= capacity && b + Geometry.size n <= capacity && 0 <= d && d <= capacity && d + Geometry.size m <= capacity
      && stop = base + 16 + 16 * capacity} ->
    {u : unit | Copy.bounded plan base base stop} @ ghost = fun plan a b n c d m base capacity stop premise -> ghost_ (
      Geometry.two_def plan a b n c d m; Copy.bounded_def Plan.End base base stop;
      match Geometry.split plan a b n with
      | None -> ()
      | Some rest -> split rest Plan.End c d m capacity base stop (); split plan rest a b n capacity base stop ())

let (four @ total) : (plan : Plan.plan) @ immutable -> (a : int) -> (b : int) -> (n : D.index) @ immutable ->
    (c : int) -> (d : int) -> (m : D.index) @ immutable -> (e : int) -> (f : int) -> (k : D.index) @ immutable ->
    (g : int) -> (h : int) -> (l : D.index) @ immutable -> (base : B.u32) -> (capacity : Lower.count) -> (stop : B.u32) ->
    {u : unit | Geometry.four plan a b n c d m e f k g h l && stop = base + 16 + 16 * capacity
      && Index.fits n capacity && Index.fits m capacity && Index.fits k capacity && Index.fits l capacity
      && 0 <= b && b <= capacity && b + Geometry.size n <= capacity && 0 <= d && d <= capacity && d + Geometry.size m <= capacity
      && 0 <= f && f <= capacity && f + Geometry.size k <= capacity && 0 <= h && h <= capacity && h + Geometry.size l <= capacity} ->
    {u : unit | Copy.bounded plan base base stop} @ ghost = fun plan a b n c d m e f k g h l base capacity stop premise -> ghost_ (
      Geometry.four_def plan a b n c d m e f k g h l;
      match Geometry.split plan a b n with
      | None -> ()
      | Some second -> match Geometry.split second c d m with
        | None -> ()
        | Some third ->
          two third e f k g h l base capacity stop ();
          split second third c d m capacity base stop ();
          split plan second a b n capacity base stop ())

let (correct @ total) : (signature : G.signature) @ immutable -> (instruction : G.instruction) @ immutable ->
    (capacity : Lower.count) -> (max_pc : B.u32) -> (fragment : Lower.fragment) @ immutable -> (base : B.u32) -> (stop : B.u32) ->
    {u : unit | Geometry.matches signature instruction capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required
      && stop = base + 16 + 16 * capacity} ->
    {u : unit | Copy.bounded fragment.Lower.copies base base stop} @ ghost =
  fun signature instruction capacity max_pc fragment base stop premise -> ghost_ (
    let env_size = Codec.locals_size signature.G.locals in let temp_size = Codec.temporaries_size signature.G.temporaries in
    Geometry.matches_def signature instruction capacity max_pc fragment.Lower.copies fragment.Lower.pc fragment.Lower.required;
    let env = Counts.encode env_size capacity () in let _ = Counts.encode temp_size capacity () in
    match instruction with
    | G.Save_environment _ -> two fragment.Lower.copies 2 (2 + env) env_size (2 + env) (2 + 2 * env) temp_size base capacity stop ()
    | G.Save_value _ | G.Bind _ | G.Restore _ -> (match signature.G.temporaries with
      | G.Environment (context, schema) ->
        let saved_size = Codec.locals_size context in let rest_size = Codec.temporaries_size schema in
        let saved = Counts.encode saved_size capacity () in let _ = Counts.encode rest_size capacity () in
        Index.fits_def (D.S D.Z) capacity; Index.fits_def D.Z (capacity - 1);
        Geometry.size_def (D.S D.Z); Geometry.size_def D.Z;
        (match instruction with
        | G.Restore _ -> two fragment.Lower.copies (2 + env) 2 saved_size (2 + env + saved) (2 + saved) rest_size base capacity stop ()
        | G.Save_value _ -> four fragment.Lower.copies (2 + env) 2 saved_size 1 (2 + saved) (D.S D.Z)
            (2 + env) (3 + saved) saved_size (2 + env + saved) (3 + 2 * saved) rest_size base capacity stop ()
        | _ -> four fragment.Lower.copies 1 2 (D.S D.Z) (2 + env) 3 saved_size
            (2 + env) (3 + saved) saved_size (2 + env + saved) (3 + 2 * saved) rest_size base capacity stop ())
      | _ -> ())
    | _ -> ())
