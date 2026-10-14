open Vox_smt

type call =
  { int_width : int;
    timeout_ms : int;
    resource_limit : int option;
    assumptions : Symbol.t list option;
    query : query;
    request_model : bool
  }

let ops =
  [| Add;
     Sub;
     Mul;
     Div;
     Rem;
     Neg;
     Bit_and;
     Bit_or;
     Bit_xor;
     Shift_right_logical;
     Shift_left;
     Shift_right_arithmetic;
     Eq;
     Ne;
     Lt;
     Le;
     Gt;
     Ge;
     Not;
     And;
     Or;
     Implies;
     Ite;
     Int_add;
     Int_sub;
     Int_mul;
     Int_div;
     Int_mod;
     Int_neg;
     Int_lt;
     Int_le;
     Int_gt;
     Int_ge;
     Int_of_int63;
     Int63_of_int
  |]

let invalid () = invalid_arg "Malformed Vox API query capture"

let put_int out n =
  output_string out (string_of_int n);
  output_char out '\n'

let put_string out s =
  put_int out (String.length s);
  output_string out s

let get_int input =
  match int_of_string_opt (input_line input) with
  | Some n -> n
  | None -> invalid ()

let count input =
  let n = get_int input in
  if n < 0 then invalid ();
  n

let get_string input = really_input_string input (count input)

let get_ref input values =
  let index = count input in
  if index >= Array.length values then invalid ();
  values.(index)

let put_list put out values =
  put_int out (List.length values);
  List.iter (put out) values

let get_list get input = List.init (count input) (fun _ -> get input)

let get_bool input =
  match get_int input with 0 -> false | 1 -> true | _ -> invalid ()

module Terms = Hashtbl.Make (struct
  type t = term

  let equal = ( == )

  let hash = Hashtbl.hash
end)

let indices values =
  let table = Hashtbl.create (List.length values) in
  List.iteri (fun i value -> Hashtbl.add table value i) values;
  table

let write_portable_call out call =
  let q = call.query in
  let datatypes = indices (List.map (fun d -> d.datatype) q.datatypes) in
  let symbols = indices q.symbols and functions = indices q.functions in
  let constructors =
    indices (List.concat_map (fun d -> d.constructors) q.datatypes)
  in
  let sort out = function
    | Bool -> put_int out 0
    | Int63 -> put_int out 1
    | Int -> put_int out 2
    | Opaque n ->
      put_int out 3;
      put_int out n
    | Datatype d ->
      put_int out 4;
      put_int out (Hashtbl.find datatypes d)
  in
  let terms = Terms.create 128 and ordered = ref [] and next = ref 0 in
  let rec visit term =
    if not (Terms.mem terms term)
    then begin
      (match term with
      | Boolean _ | Integer _ | Big_integer _ | Var _ -> ()
      | App (_, args) | Call (_, args) | Construct (_, args) ->
        List.iter visit args
      | Is (_, term) | Select (_, _, term) -> visit term);
      Terms.add terms term !next;
      incr next;
      ordered := term :: !ordered
    end
  in
  List.iter (fun f -> visit f.term) q.facts;
  visit q.goal.term;
  let ref_term out term = put_int out (Terms.find terms term) in
  let ref_constructor out c = put_int out (Hashtbl.find constructors c) in
  output_string out "vox-api-query-v2\n";
  put_int out call.int_width;
  put_int out call.timeout_ms;
  put_int out (Option.value call.resource_limit ~default:(-1));
  put_int out (if call.request_model then 1 else 0);
  put_list
    (fun out d -> put_string out (Datatype.label d.datatype))
    out q.datatypes;
  List.iter
    (fun d ->
      put_list
        (fun out c ->
          put_string out (Constructor.label c);
          put_list
            (fun out (label, field) ->
              put_string out label;
              sort out field)
            out (Constructor.fields c))
        out d.constructors)
    q.datatypes;
  put_list
    (fun out symbol ->
      put_string out (Symbol.label symbol);
      sort out (Symbol.sort symbol))
    out q.symbols;
  put_list
    (fun out f ->
      put_string out (Function.label f);
      put_list sort out (Function.arguments f);
      sort out (Function.result f))
    out q.functions;
  (match call.assumptions with
  | None -> put_int out (-1)
  | Some values ->
    put_list (fun out s -> put_int out (Hashtbl.find symbols s)) out values);
  put_list
    (fun out -> function
      | Boolean b ->
        put_int out 0;
        put_int out (if b then 1 else 0)
      | Integer n ->
        put_int out 1;
        put_string out (Int64.to_string n)
      | Big_integer n ->
        put_int out 2;
        put_string out n
      | Var symbol ->
        put_int out 3;
        put_int out (Hashtbl.find symbols symbol)
      | App (op, args) ->
        let rec find index =
          if index = Array.length ops then invalid ();
          if op = ops.(index) then index else find (index + 1)
        in
        put_int out 4;
        put_int out (find 0);
        put_list ref_term out args
      | Call (f, args) ->
        put_int out 5;
        put_int out (Hashtbl.find functions f);
        put_list ref_term out args
      | Construct (c, args) ->
        put_int out 6;
        ref_constructor out c;
        put_list ref_term out args
      | Is (c, term) ->
        put_int out 7;
        ref_constructor out c;
        ref_term out term
      | Select (c, index, term) ->
        put_int out 8;
        ref_constructor out c;
        put_int out index;
        ref_term out term)
    out (List.rev !ordered);
  let labelled out term =
    put_string out term.label;
    ref_term out term.term
  in
  put_list labelled out q.facts;
  labelled out q.goal

module Local_graph = struct
  type value =
    | Immediate of int
    | Reference of int

  type node =
    | Pending
    | Block of int * value array
    | Text of string
    | Int64_value of int64

  type t =
    { root : value;
      nodes : node array
    }

  let invalid () = invalid_arg "Malformed Vox local query graph"

  let read input =
    let bits input bytes =
      let n = ref 0L in
      for _ = 1 to bytes do
        n
          := Int64.logor (Int64.shift_left !n 8)
               (Int64.of_int (input_byte input))
      done;
      !n
    in
    let size n =
      if n < 0L || n > Int64.of_int max_int then invalid ();
      Int64.to_int n
    in
    let magic = bits input 4 in
    let bytes, objects =
      if magic = 0x8495a6beL
      then begin
        let bytes = size (bits input 4) in
        let objects = size (bits input 4) in
        ignore (bits input 4);
        ignore (bits input 4);
        bytes, objects
      end
      else if magic = 0x8495a6bfL
      then begin
        if bits input 4 <> 0L then invalid ();
        let bytes = size (bits input 8) in
        let objects = size (bits input 8) in
        ignore (bits input 8);
        bytes, objects
      end
      else invalid ()
    in
    if objects > bytes || objects > Sys.max_array_length then invalid ();
    let data = really_input_string input bytes in
    let position = ref 0 in
    let byte () =
      if !position = bytes then invalid ();
      let n = Char.code data.[!position] in
      incr position;
      n
    in
    let bits bytes =
      let n = ref 0L in
      for _ = 1 to bytes do
        n := Int64.logor (Int64.shift_left !n 8) (Int64.of_int (byte ()))
      done;
      !n
    in
    let integer bytes =
      let n = bits bytes in
      let shift = 64 - (8 * bytes) in
      let n = Int64.shift_right (Int64.shift_left n shift) shift in
      if n < Int64.of_int min_int || n > Int64.of_int max_int then invalid ();
      Immediate (Int64.to_int n)
    in
    let text length =
      if length > bytes - !position then invalid ();
      let text = String.sub data !position length in
      position := !position + length;
      text
    in
    let nodes = Array.make objects Pending in
    let complete = Array.make objects false in
    let next = ref 0 and finished = ref 0 in
    let finish id =
      complete.(id) <- true;
      incr finished
    in
    let add node =
      if !next = objects then invalid ();
      let id = !next in
      incr next;
      nodes.(id) <- node;
      id
    in
    let leaf node =
      let id = add node in
      finish id;
      Reference id, None
    in
    let block tag count =
      if count = 0 || count > bytes - !position || count > Sys.max_array_length
      then invalid ();
      let fields = Array.make count (Immediate 0) in
      let id = add (Block (tag, fields)) in
      Reference id, Some (id, fields, 0)
    in
    let shared count =
      if count <= 0 || count > !next then invalid ();
      let id = !next - count in
      if not complete.(id) then invalid ();
      Reference id, None
    in
    let value () =
      let code = byte () in
      if code >= 0x80
      then block (code land 15) ((code lsr 4) land 7)
      else if code >= 0x40
      then Immediate (code land 63), None
      else if code >= 0x20
      then leaf (Text (text (code land 31)))
      else
        match code with
        | 0x00 -> integer 1, None
        | 0x01 -> integer 2, None
        | 0x02 -> integer 4, None
        | 0x03 -> integer 8, None
        | 0x04 -> shared (size (bits 1))
        | 0x05 -> shared (size (bits 2))
        | 0x06 -> shared (size (bits 4))
        | 0x14 -> shared (size (bits 8))
        | 0x08 | 0x13 ->
          let header = bits (if code = 0x08 then 4 else 8) in
          block
            (Int64.to_int (Int64.logand header 255L))
            (size (Int64.shift_right_logical header 10))
        | 0x09 -> leaf (Text (text (size (bits 1))))
        | 0x0a -> leaf (Text (text (size (bits 4))))
        | 0x15 -> leaf (Text (text (size (bits 8))))
        | 0x19 ->
          if
            byte () <> Char.code '_' || byte () <> Char.code 'j' || byte () <> 0
          then invalid ();
          leaf (Int64_value (bits 8))
        | _ -> invalid ()
    in
    let root, frame = value () in
    let pending = ref (Option.to_list frame) in
    while !pending <> [] do
      match !pending with
      | [] -> ()
      | (id, fields, index) :: rest ->
        if index = Array.length fields
        then begin
          finish id;
          pending := rest
        end
        else begin
          let field, frame = value () in
          fields.(index) <- field;
          let rest = (id, fields, index + 1) :: rest in
          pending
            := match frame with None -> rest | Some frame -> frame :: rest
        end
    done;
    if !position <> bytes || !next <> objects || !finished <> objects
    then invalid ();
    { root; nodes }
end

module Local_query = struct
  (* Handles are process-local. Rebuild each handle once per object identity, so
     captures from different compiler processes can be combined safely. *)
  open Local_graph

  type _ kind =
    | Integer : int kind
    | Natural : int kind
    | Positive : int kind
    | Boolean : bool kind
    | Operator : op kind
    | String : string kind
    | Int64 : int64 kind
    | Sort : sort kind
    | Sorts : sort list kind
    | Datatype : Datatype.t kind
    | Constructor : Constructor.t kind
    | Constructors : Constructor.t list kind
    | Field : (string * sort) kind
    | Fields : (string * sort) list kind
    | Symbol : Symbol.t kind
    | Symbols : Symbol.t list kind
    | Function : Function.t kind
    | Functions : Function.t list kind
    | Term : term kind
    | Terms : term list kind
    | Labelled : labelled_term kind
    | Facts : labelled_term list kind
    | Declaration : datatype_declaration kind
    | Declarations : datatype_declaration list kind
    | Query : query kind
    | Limit : int option kind
    | Assumptions : Symbol.t list option kind
    | Call : call kind

  type (_, _) equality = Equal : ('a, 'a) equality

  let same : type a b. a kind -> b kind -> (a, b) equality option =
   fun a b ->
    match a, b with
    | Integer, Integer -> Some Equal
    | Natural, Natural -> Some Equal
    | Positive, Positive -> Some Equal
    | Boolean, Boolean -> Some Equal
    | Operator, Operator -> Some Equal
    | String, String -> Some Equal
    | Int64, Int64 -> Some Equal
    | Sort, Sort -> Some Equal
    | Sorts, Sorts -> Some Equal
    | Datatype, Datatype -> Some Equal
    | Constructor, Constructor -> Some Equal
    | Constructors, Constructors -> Some Equal
    | Field, Field -> Some Equal
    | Fields, Fields -> Some Equal
    | Symbol, Symbol -> Some Equal
    | Symbols, Symbols -> Some Equal
    | Function, Function -> Some Equal
    | Functions, Functions -> Some Equal
    | Term, Term -> Some Equal
    | Terms, Terms -> Some Equal
    | Labelled, Labelled -> Some Equal
    | Facts, Facts -> Some Equal
    | Declaration, Declaration -> Some Equal
    | Declarations, Declarations -> Some Equal
    | Query, Query -> Some Equal
    | Limit, Limit -> Some Equal
    | Assumptions, Assumptions -> Some Equal
    | Call, Call -> Some Equal
    | _ -> None

  type 'a plan =
    | Return of 'a
    | Read : 'b kind * value * ('b -> 'a plan) -> 'a plan

  let read kind value = Read (kind, value, fun value -> Return value)

  let rec ( let* ) plan f =
    match plan with
    | Return value -> f value
    | Read (kind, value, rest) ->
      Read
        ( kind,
          value,
          fun value ->
            let* value = rest value in
            f value )

  let immediate : type a. a kind -> int -> a =
   fun kind value ->
    match kind, value with
    | Integer, value -> value
    | Natural, value when value >= 0 -> value
    | Positive, value when value > 0 -> value
    | Boolean, 0 -> false
    | Boolean, 1 -> true
    | Operator, value when value >= 0 && value < Array.length ops -> ops.(value)
    | Sort, 0 -> Bool
    | Sort, 1 -> Int63
    | Sort, 2 -> Int
    | Sorts, 0 -> []
    | Constructors, 0 -> []
    | Fields, 0 -> []
    | Symbols, 0 -> []
    | Functions, 0 -> []
    | Terms, 0 -> []
    | Facts, 0 -> []
    | Declarations, 0 -> []
    | Limit, 0 -> None
    | Assumptions, 0 -> None
    | _ -> invalid ()

  let list head tail first rest =
    let* first = read head first in
    let* rest = read tail rest in
    Return (first :: rest)

  let node : type a. a kind -> node -> a plan =
   fun kind node ->
    match kind, node with
    | String, Text text -> Return text
    | Int64, Int64_value value -> Return value
    | Sort, Block (0, [| value |]) ->
      let* value = read Integer value in
      Return (Opaque value)
    | Sort, Block (1, [| value |]) ->
      let* value = read Datatype value in
      Return (Vox_smt.Datatype value)
    | Sorts, Block (0, [| head; tail |]) -> list Sort Sorts head tail
    | Constructors, Block (0, [| head; tail |]) ->
      list Constructor Constructors head tail
    | Fields, Block (0, [| head; tail |]) -> list Field Fields head tail
    | Symbols, Block (0, [| head; tail |]) -> list Symbol Symbols head tail
    | Functions, Block (0, [| head; tail |]) ->
      list Function Functions head tail
    | Terms, Block (0, [| head; tail |]) -> list Term Terms head tail
    | Facts, Block (0, [| head; tail |]) -> list Labelled Facts head tail
    | Declarations, Block (0, [| head; tail |]) ->
      list Declaration Declarations head tail
    | Datatype, Block (0, [| id; label |]) ->
      let* _ = read Natural id in
      let* label = read String label in
      Return (Datatype.create ~label)
    | Constructor, Block (0, [| id; datatype; label; fields |]) ->
      let* _ = read Natural id in
      let* datatype = read Datatype datatype in
      let* label = read String label in
      let* fields = read Fields fields in
      Return (Constructor.create ~datatype ~label fields)
    | Field, Block (0, [| label; sort |]) ->
      let* label = read String label in
      let* sort = read Sort sort in
      Return (label, sort)
    | Symbol, Block (0, [| id; label; sort |]) ->
      let* _ = read Natural id in
      let* label = read String label in
      let* sort = read Sort sort in
      Return (Symbol.create ~label sort)
    | Function, Block (0, [| id; label; arguments; result |]) ->
      let* _ = read Natural id in
      let* label = read String label in
      let* arguments = read Sorts arguments in
      let* result = read Sort result in
      Return (Function.create ~label ~arguments ~result)
    | Term, Block (0, [| value |]) ->
      let* value = read Boolean value in
      Return (Vox_smt.Boolean value)
    | Term, Block (1, [| value |]) ->
      let* value = read Int64 value in
      Return (Vox_smt.Integer value)
    | Term, Block (2, [| value |]) ->
      let* value = read String value in
      Return (Big_integer value)
    | Term, Block (3, [| value |]) ->
      let* value = read Symbol value in
      Return (Var value)
    | Term, Block (4, [| op; args |]) ->
      let* op = read Operator op in
      let* args = read Terms args in
      Return (App (op, args))
    | Term, Block (5, [| function_; args |]) ->
      let* function_ = read Function function_ in
      let* args = read Terms args in
      Return (Vox_smt.Call (function_, args))
    | Term, Block (6, [| constructor; args |]) ->
      let* constructor = read Constructor constructor in
      let* args = read Terms args in
      Return (Construct (constructor, args))
    | Term, Block (7, [| constructor; value |]) ->
      let* constructor = read Constructor constructor in
      let* value = read Term value in
      Return (Is (constructor, value))
    | Term, Block (8, [| constructor; index; value |]) ->
      let* constructor = read Constructor constructor in
      let* index = read Natural index in
      let* value = read Term value in
      Return (Select (constructor, index, value))
    | Labelled, Block (0, [| label; term |]) ->
      let* label = read String label in
      let* term = read Term term in
      Return { label; term }
    | Declaration, Block (0, [| datatype; constructors |]) ->
      let* datatype = read Datatype datatype in
      let* constructors = read Constructors constructors in
      Return { datatype; constructors }
    | Query, Block (0, [| datatypes; symbols; functions; facts; goal |]) ->
      let* datatypes = read Declarations datatypes in
      let* symbols = read Symbols symbols in
      let* functions = read Functions functions in
      let* facts = read Facts facts in
      let* goal = read Labelled goal in
      Return { datatypes; symbols; functions; facts; goal }
    | Limit, Block (0, [| value |]) ->
      let* value = read Positive value in
      Return (Some value)
    | Assumptions, Block (0, [| value |]) ->
      let* value = read Symbols value in
      Return (Some value)
    | ( Call,
        Block
          (0, [| int_width; timeout_ms; resource_limit; assumptions; query |]) )
      ->
      let* int_width = read Natural int_width in
      let* timeout_ms = read Natural timeout_ms in
      let* resource_limit = read Limit resource_limit in
      let* assumptions = read Assumptions assumptions in
      let* query = read Query query in
      Return { int_width; timeout_ms; resource_limit; assumptions; query;
        request_model = true }
    | ( Call,
        Block (0, [| int_width; timeout_ms; resource_limit; assumptions;
                     query; request_model |]) ) ->
      let* int_width = read Natural int_width in
      let* timeout_ms = read Natural timeout_ms in
      let* resource_limit = read Limit resource_limit in
      let* assumptions = read Assumptions assumptions in
      let* query = read Query query in
      let* request_model = read Boolean request_model in
      Return { int_width; timeout_ms; resource_limit; assumptions; query;
        request_model }
    | _ -> invalid ()

  type packed = Value : 'a kind * 'a -> packed

  type (_, _) continuation =
    | Finish : ('a, 'a) continuation
    | Bind :
        ('a -> 'b plan) * ('b, 'result) continuation
        -> ('a, 'result) continuation
    | Save :
        int * 'a kind * ('a, 'result) continuation
        -> ('a, 'result) continuation

  let rec find : type a. a kind -> packed list -> a option =
   fun kind values ->
    match values with
    | [] -> None
    | Value (previous_kind, value) :: rest -> (
      match same kind previous_kind with
      | Some Equal -> Some value
      | None -> find kind rest)

  let decode graph =
    (* The compiler can share constants across types with equal layouts. *)
    let values = Array.make (Array.length graph.nodes) [] in
    let rec evaluate : type a result.
        a plan -> (a, result) continuation -> result =
     fun plan continuation ->
      match plan with
      | Return value -> resume value continuation
      | Read (kind, value, rest) ->
        request kind value (Bind (rest, continuation))
    and request : type a result.
        a kind -> value -> (a, result) continuation -> result =
     fun kind value continuation ->
      match value with
      | Immediate value -> resume (immediate kind value) continuation
      | Reference id -> (
        match find kind values.(id) with
        | Some value -> resume value continuation
        | None ->
          evaluate (node kind graph.nodes.(id)) (Save (id, kind, continuation)))
    and resume : type a result. a -> (a, result) continuation -> result =
     fun value continuation ->
      match continuation with
      | Finish -> value
      | Bind (rest, continuation) -> evaluate (rest value) continuation
      | Save (id, kind, continuation) ->
        values.(id) <- Value (kind, value) :: values.(id);
        resume value continuation
    in
    request Call graph.root Finish
end

(* Bump this version when [call] or any serialized [Vox_smt] layout changes.
   Only the standard writer handles runtime values; the reader checks the object
   graph and rebuilds handles with fresh identities. *)
let local_version = "vox-api-query-local-v3"

let write_call out (call : call) =
  output_string out local_version;
  output_char out '\n';
  output_string out Sys.ocaml_version;
  output_char out '\n';
  Marshal.to_channel out call []

let read_call input =
  match input_line input with
  | exception End_of_file -> None
  | version when version = local_version
                 || version = "vox-api-query-local-v2" ->
    if input_line input <> Sys.ocaml_version then invalid ();
    Some (Local_query.decode (Local_graph.read input))
  | ("vox-api-query-v1" | "vox-api-query-v2") as version ->
    let int_width = count input in
    let timeout_ms = count input in
    let resource_limit =
      match get_int input with
      | -1 -> None
      | n when n > 0 -> Some n
      | _ -> invalid ()
    in
    let request_model =
      version = "vox-api-query-v1" || get_bool input in
    let datatypes =
      Array.of_list
        (get_list
           (fun input -> Datatype.create ~label:(get_string input))
           input)
    in
    let sort input =
      match get_int input with
      | 0 -> Bool
      | 1 -> Int63
      | 2 -> Int
      | 3 -> Opaque (get_int input)
      | 4 -> Datatype (get_ref input datatypes)
      | _ -> invalid ()
    in
    let declarations =
      Array.to_list
        (Array.map
           (fun datatype ->
             let constructors =
               get_list
                 (fun input ->
                   let label = get_string input in
                   let fields =
                     get_list
                       (fun input ->
                         let label = get_string input in
                         label, sort input)
                       input
                   in
                   Constructor.create ~datatype ~label fields)
                 input
             in
             { datatype; constructors })
           datatypes)
    in
    let constructors =
      Array.of_list (List.concat_map (fun d -> d.constructors) declarations)
    in
    let symbols =
      Array.of_list
        (get_list
           (fun input ->
             let label = get_string input in
             Symbol.create ~label (sort input))
           input)
    in
    let functions =
      Array.of_list
        (get_list
           (fun input ->
             let label = get_string input in
             let arguments = get_list sort input in
             let result = sort input in
             Function.create ~label ~arguments ~result)
           input)
    in
    let assumptions =
      match get_int input with
      | -1 -> None
      | n when n >= 0 -> Some (List.init n (fun _ -> get_ref input symbols))
      | _ -> invalid ()
    in
    let terms = Array.make (count input) (Boolean false) in
    Array.iteri
      (fun index _ ->
        let ref_term input =
          let child = count input in
          if child >= index then invalid ();
          terms.(child)
        in
        let term =
          match get_int input with
          | 0 -> Boolean (get_bool input)
          | 1 -> Integer (Int64.of_string (get_string input))
          | 2 -> Big_integer (get_string input)
          | 3 -> Var (get_ref input symbols)
          | 4 ->
            let op = get_ref input ops in
            App (op, get_list ref_term input)
          | 5 ->
            let f = get_ref input functions in
            Call (f, get_list ref_term input)
          | 6 ->
            let c = get_ref input constructors in
            Construct (c, get_list ref_term input)
          | 7 ->
            let c = get_ref input constructors in
            Is (c, ref_term input)
          | 8 ->
            let c = get_ref input constructors in
            let field = count input in
            Select (c, field, ref_term input)
          | _ -> invalid ()
        in
        terms.(index) <- term)
      terms;
    let labelled input =
      let label = get_string input in
      { label; term = get_ref input terms }
    in
    let facts = get_list labelled input in
    let goal = labelled input in
    Some
      { int_width;
        timeout_ms;
        resource_limit;
        assumptions;
        request_model;
        query =
          { datatypes = declarations;
            symbols = Array.to_list symbols;
            functions = Array.to_list functions;
            facts;
            goal
          }
      }
  | _ -> invalid ()
