module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module C = Wasm_word_memory
module Q = Wasm_word_sequence
module H = Hmc_wasm_header_words
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
let (parts @ total) : (bytes : B.bytes) @ immutable -> (value : V.value) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | V.decode bytes === Some (value, tail)} ->
    {middle : B.bytes | C.decode bytes === Some (V.tag value, middle)
      && C.decode middle === Some (V.payload value, tail)} @ immutable = fun bytes value tail premise ->
  ghost_ (V.decode_def bytes; V.tag_def value; V.payload_def value);
  match C.decode bytes with
  | None -> unreachable_ ()
  | Some (_, middle) ->
    ghost_ (match C.decode middle with None -> () | Some (payload, _) ->
      let _lo : W.limb = payload.W.lo in let _hi : W.limb = payload.W.hi in ());
    middle
let (cell_unique @ total) : (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    (value : V.value) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | V.decode left === Some (value, tail) && V.decode right === Some (value, tail)} ->
    {u : unit | left === right} @ ghost = fun left right value tail premise -> ghost_ (
    let a = parts left value tail () in let b = parts right value tail () in
    Wasm_word_prefix.unique a b (V.payload value) tail ();
    Wasm_word_prefix.unique left right (V.tag value) a ())
let rec (cells_unique @ total) : (count : D.index) @ immutable -> (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    (cells : Heap.cells) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | Wire.decode_cells count left === Some (cells, tail) && Wire.decode_cells count right === Some (cells, tail)} ->
    {u : unit | left === right} @ ghost = fun count left right cells tail premise -> ghost_ (
    Wire.decode_cells_def count left; Wire.decode_cells_def count right;
    match count with
    | D.Z -> ()
    | D.S rest -> (match V.decode left, V.decode right with
      | Some (value, a), Some (_, b) -> (match cells with
        | Heap.Empty -> ()
        | Heap.Cell (_, remaining) -> cells_unique rest a b remaining tail (); cell_unique left right value a ())
      | _ -> ()))
let (header @ total) : (count : D.index) @ immutable -> (bytes : B.bytes) @ immutable -> (pc : W.limb) ->
    (current : V.value) @ immutable -> (accumulator : V.value) @ immutable -> (cells : Heap.cells) @ immutable ->
    (tail : B.bytes) @ immutable ->
    {u : unit | Wire.decode (Wire.Closure_schema (D.S (D.S count))) bytes
      === Some (Wire.Closure (pc, Heap.Cell (current, Heap.Cell (accumulator, cells))), tail)} ->
    {suffix : B.bytes | Q.decode (H.layout (Hmc_wasm_header_update.number pc)
      (V.tag current) (V.payload current) (V.tag accumulator) (V.payload accumulator)) bytes === Some suffix
      && Wire.decode_cells count suffix === Some (cells, tail)} @ immutable =
  fun count bytes pc current accumulator cells tail premise ->
    ghost_ (Wire.decode_def (Wire.Closure_schema (D.S (D.S count))) bytes);
    match V.decode bytes with
    | Some (V.Word code, after_pc) ->
      ghost_ (Wire.decode_cells_def (D.S (D.S count)) after_pc);
      (match V.decode after_pc with
      | None -> unreachable_ ()
      | Some (_, after_current) ->
        ghost_ (Wire.decode_cells_def (D.S count) after_current);
        (match V.decode after_current with
        | None -> unreachable_ ()
        | Some (_, suffix) ->
          let pc_middle = parts bytes (V.Word code) after_pc () in
          let current_middle = parts after_pc current after_current () in
          let acc_middle = parts after_current accumulator suffix () in
          ghost_ (
            Hmc_wasm_header_update.number_def pc; H.layout_def code (V.tag current) (V.payload current) (V.tag accumulator) (V.payload accumulator);
            H.tag_def (); V.tag_def (V.Word code); V.payload_def (V.Word code);
            let rest5 = Q.Word (code, Q.Word (V.tag current, Q.Word (V.payload current, Q.Word (V.tag accumulator, Q.Word (V.payload accumulator, Q.End))))) in
            Q.decode_def (H.layout code (V.tag current) (V.payload current) (V.tag accumulator) (V.payload accumulator)) bytes;
            Q.decode_def rest5 pc_middle;
            Q.decode_def (Q.Word (V.tag current, Q.Word (V.payload current, Q.Word (V.tag accumulator, Q.Word (V.payload accumulator, Q.End))))) after_pc;
            Q.decode_def (Q.Word (V.payload current, Q.Word (V.tag accumulator, Q.Word (V.payload accumulator, Q.End)))) current_middle;
            Q.decode_def (Q.Word (V.tag accumulator, Q.Word (V.payload accumulator, Q.End))) after_current;
            Q.decode_def (Q.Word (V.payload accumulator, Q.End)) acc_middle;
            Q.decode_def Q.End suffix;
            W.equal_def (H.tag ()) (H.tag ()); W.equal_def code code;
            W.equal_def (V.tag current) (V.tag current); W.equal_def (V.payload current) (V.payload current);
            W.equal_def (V.tag accumulator) (V.tag accumulator); W.equal_def (V.payload accumulator) (V.payload accumulator));
          suffix))
    | _ -> unreachable_ ()
