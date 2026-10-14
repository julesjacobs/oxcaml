module Z = Vox_smt_integer.Z
module Q = Vox_smt_integer.Q
module Int_map = Map.Make (Int)
module Int_set = Set.Make (Int)
module Int_table = Hashtbl.Make (struct
  type t = int
  let equal = Int.equal
  let hash x = x land max_int
end)


module Bitmap = struct
  type t =
    { mutable words : int array; mutable length : int;
      mutable first_word : int; mutable last_word : int }
  let create () =
    { words = [||]; length = 0; first_word = 0; last_word = -1 }
  let cardinal t = t.length
  let word_bits = if Sys.int_size >= 63 then 32 else 16
  let add member t =
    let index = member / word_bits in
    if index >= Array.length t.words then begin
      let words = Array.make (max (index + 1)
          (max 4 (2 * Array.length t.words))) 0 in
      Array.blit t.words 0 words 0 (Array.length t.words);
      t.words <- words
    end;
    let mask = 1 lsl (member mod word_bits) in
    let previous = t.words.(index) in
    if previous land mask = 0 then begin
      t.words.(index) <- previous lor mask;
      if t.length = 0 then begin
        t.first_word <- index; t.last_word <- index
      end else begin
        t.first_word <- min t.first_word index;
        t.last_word <- max t.last_word index
      end;
      t.length <- t.length + 1
    end

  let remove member t =
    let index = member / word_bits in
    if index < Array.length t.words then begin
      let mask = 1 lsl (member mod word_bits) in
      let previous = t.words.(index) in
      if previous land mask <> 0 then begin
        t.words.(index) <- previous land (lnot mask);
        t.length <- t.length - 1;
        if t.length = 0 then begin
          t.first_word <- 0; t.last_word <- -1
        end else begin
          while t.words.(t.first_word) = 0 do
            t.first_word <- t.first_word + 1
          done;
          while t.words.(t.last_word) = 0 do
            t.last_word <- t.last_word - 1
          done
        end
      end
    end

  let first_fallback word =
    let offset = ref 0 and word = ref word in
    if !word land 65535 = 0 then begin
      offset := 16; word := !word lsr 16
    end;
    if !word land 255 = 0 then begin
      offset := !offset + 8; word := !word lsr 8
    end;
    if !word land 15 = 0 then begin
      offset := !offset + 4; word := !word lsr 4
    end;
    if !word land 3 = 0 then begin
      offset := !offset + 2; word := !word lsr 2
    end;
    !offset + if !word land 1 = 0 then 1 else 0

  let trailing = [|0; 1; 28; 2; 29; 14; 24; 3;
    30; 22; 20; 15; 25; 17; 4; 8;
    31; 27; 13; 23; 21; 19; 16; 7;
    26; 12; 18; 6; 11; 5; 10; 9|]

  let first word =
    if Sys.int_size >= 63 then
      trailing.((((word land (-word)) * 0x077cb531) lsr 27) land 31)
    else first_fallback word

  let iter f t =
    for index = t.first_word to t.last_word do
      let word = ref t.words.(index) in
      while !word <> 0 do
        f ((index * word_bits) + first !word);
        word := !word land (!word - 1)
      done
    done

  let fold f t initial =
    let result = ref initial in
    iter (fun member -> result := f member !result) t;
    !result
end

module Column = struct
  type contents = Sparse of Int_set.t | Dense of Bitmap.t
  type t = { mutable contents : contents; mutable length : int }

  let create () = { contents = Sparse Int_set.empty; length = 0 }
  let cardinal t = t.length

  let sparse bitmap =
    Sparse (Bitmap.fold Int_set.add bitmap Int_set.empty)

  let add member t =
    match t.contents with
    | Sparse members ->
      let updated = Int_set.add member members in
      if updated != members then begin
        t.length <- t.length + 1;
        if t.length >= 4
           && Int_set.max_elt updated / Bitmap.word_bits
              <= (4 * t.length) + 8
        then begin
          let bitmap = Bitmap.create () in
          Int_set.iter (fun member -> Bitmap.add member bitmap) updated;
          t.contents <- Dense bitmap
        end else t.contents <- Sparse updated
      end
    | Dense bitmap ->
      let index = member / Bitmap.word_bits in
      if index >= Array.length bitmap.words
         && index > (4 * (t.length + 1)) + 8
      then begin
        let members = Bitmap.fold Int_set.add bitmap Int_set.empty in
        t.contents <- Sparse (Int_set.add member members);
        t.length <- t.length + 1
      end else begin
        Bitmap.add member bitmap;
        t.length <- Bitmap.cardinal bitmap
      end

  let remove member t =
    match t.contents with
    | Sparse members ->
      let updated = Int_set.remove member members in
      if updated != members then begin
        t.length <- t.length - 1;
        t.contents <- Sparse updated
      end
    | Dense bitmap ->
      Bitmap.remove member bitmap;
      t.length <- Bitmap.cardinal bitmap;
      if Array.length bitmap.words > (16 * (t.length + 1)) + 32
      then t.contents <- sparse bitmap

  let iter f t =
    match t.contents with
    | Sparse members -> Int_set.iter f members
    | Dense bitmap -> Bitmap.iter f bitmap

  let fold f t initial =
    match t.contents with
    | Sparse members -> Int_set.fold f members initial
    | Dense bitmap -> Bitmap.fold f bitmap initial
end

module Work_set = struct
  type t = { members : Bitmap.t; mutable first_word : int }
  let create () = { members = Bitmap.create (); first_word = 0 }
  let add member t =
    let empty = Bitmap.cardinal t.members = 0 in
    Bitmap.add member t.members;
    let index = member / Bitmap.word_bits in
    if empty || index < t.first_word then t.first_word <- index
  let remove member t = Bitmap.remove member t.members
  let min_elt_opt t =
    if Bitmap.cardinal t.members = 0 then None
    else begin
      while t.members.words.(t.first_word) = 0 do
        t.first_word <- t.first_word + 1
      done;
      Some ((t.first_word * Bitmap.word_bits)
            + Bitmap.first t.members.words.(t.first_word))
    end
end

module Row = struct
  type t = { indices : int array; values : Q.t array; length : int }

  let empty = { indices = [||]; values = [||]; length = 0 }
  let cardinal row = row.length

  let of_map ~poll map =
    let length = Int_map.cardinal map in
    if length = 0 then empty else
      let indices = Array.make length 0
      and values = Array.make length Q.zero in
      let offset = ref 0 in
      Int_map.iter (fun index value ->
        poll ();
        indices.(!offset) <- index; values.(!offset) <- value;
        incr offset) map;
      { indices; values; length }

  let position index row =
    let rec search low high =
      if low >= high then low else
        let middle = low + (high - low) / 2 in
        if row.indices.(middle) < index then search (middle + 1) high
        else search low middle in
    search 0 row.length

  let find index row =
    let offset = position index row in
    if offset < row.length && row.indices.(offset) = index
    then row.values.(offset) else raise Not_found

  let remove_at offset row =
    if row.length = 1 then empty else
      let length = row.length - 1 in
      let indices = Array.make length 0 and values = Array.make length Q.zero in
      Array.blit row.indices 0 indices 0 offset;
      Array.blit row.values 0 values 0 offset;
      Array.blit row.indices (offset + 1) indices offset (length - offset);
      Array.blit row.values (offset + 1) values offset (length - offset);
      { indices; values; length }

  let remove index row =
    let offset = position index row in
    if offset < row.length && row.indices.(offset) = index
    then remove_at offset row else row

  let iter f row =
    for index = 0 to row.length - 1 do
      f row.indices.(index) row.values.(index)
    done

  let fold f row initial =
    let result = ref initial in
    for index = 0 to row.length - 1 do
      result := f row.indices.(index) row.values.(index) !result
    done;
    !result

  let add_scaled ~poll row coefficient source =
    if Q.is_zero coefficient || source.length = 0 then row else
      let capacity = row.length + source.length in
      let indices = Array.make capacity 0
      and values = Array.make capacity Q.zero in
      let left = ref 0 and right = ref 0 and length = ref 0 in
      let append index value =
        if not (Q.is_zero value) then begin
          indices.(!length) <- index; values.(!length) <- value;
          incr length
        end in
      while !left < row.length || !right < source.length do
        poll ();
        if !right = source.length
          || !left < row.length
             && row.indices.(!left) < source.indices.(!right) then begin
          append row.indices.(!left) row.values.(!left);
          incr left
        end else begin
          let index = source.indices.(!right) in
          let scaled = Q.mul ~poll coefficient source.values.(!right) in
          let value =
            if !left < row.length && row.indices.(!left) = index then begin
              let value = Q.add ~poll row.values.(!left) scaled in
              incr left; value
            end else scaled in
          append index value;
          incr right
        end
      done;
      if !length = 0 then empty else { indices; values; length = !length }

  let pivot ~poll row ~leaving ~entering ~inverse =
    let indices = Array.make row.length 0
    and values = Array.make row.length Q.zero in
    let offset = ref 0 and inserted = ref false in
    let append index value =
      if !offset >= row.length then
        invalid_arg "Vox_smt_arithmetic.Row.pivot";
      indices.(!offset) <- index; values.(!offset) <- value;
      incr offset in
    iter (fun index value ->
      poll ();
      if index <> entering then begin
        if not !inserted && leaving < index then begin
          append leaving inverse; inserted := true
        end;
        append index (Q.neg (Q.mul ~poll value inverse))
      end) row;
    if not !inserted then append leaving inverse;
    if !offset <> row.length then
      invalid_arg "Vox_smt_arithmetic.Row.pivot";
    { indices; values; length = row.length }

  let iter_difference ~removed ~added before after =
    let left = ref 0 and right = ref 0 in
    while !left < before.length || !right < after.length do
      if !right = after.length
        || !left < before.length
           && before.indices.(!left) < after.indices.(!right) then begin
        removed before.indices.(!left); incr left
      end else if !left = before.length
        || after.indices.(!right) < before.indices.(!left) then begin
        added after.indices.(!right); incr right
      end else begin
        incr left; incr right
      end
    done
end

type bound = { number : Q.t; reasons : int list }

type atom =
  { literal : int;
    variable : int;
    upper : bool;
    boundary : Q.t;
    mutable assignment : int
  }

type variable =
  { original : (int * Z.t) list;
    mutable row : Row.t option;
    column : Column.t;
    mutable value : Q.t;
    mutable lower : bound option;
    mutable upper : bound option;
    mutable intervals : (bound option * bound option) option;
    mutable missing_lower : int;
    mutable missing_upper : int;
    mutable atoms : atom list;
    mutable propagation_pending : bool
  }

type change =
  | Bounds of int * bound option * bound option
  | Assignment of atom * int
  | Inconsistent of int list option

type equality_witness_status = Unchecked | Missing | Valid

type t =
  { mutable variables : variable array;
    mutable count : int;
    external_variables : int Int_table.t;
    mutable externals : (int * int) list;
    mutable changed_bounds : Int_set.t;
    pending_feasibility : Work_set.t;
    forms : ((int * Z.t) list, int) Hashtbl.t;
    atoms : atom Int_table.t;
    implications : (int * int array Lazy.t) Int_table.t;
    mutable pending_variables : int list;
    mutable trail : change list;
    mutable trail_length : int;
    mutable scopes : int list;
    mutable inconsistent : int list option;
    mutable equality_witness : Z.t array option;
    mutable equality_witness_status : equality_witness_status
  }

type 'a solution =
  { model : (int * Q.t) list;
    split : (int * Z.t) option;
    split_form : ((int * Z.t) list * Z.t) option;
    cut : ((int * Z.t) list * Z.t * int array) option;
    propagations : (int * 'a) list
  }

type 'a check_result =
  | Conflict of int array | Consistent of 'a solution | Unknown

type consistent = int array solution
type lazy_consistent = int array Lazy.t solution
type result = int array check_result
type lazy_result = int array Lazy.t check_result

type phase = Propagation | Final
type cut_basis = Tableau | Reduced | Combined

let variable ?row original =
  { original; row;
    column = Column.create ();
    value = Q.zero;
    lower = None;
    upper = None;
    intervals = None;
    missing_lower = -1;
    missing_upper = -1;
    atoms = [];
    propagation_pending = false
  }

let create () =
  { variables = [||];
    count = 0;
    external_variables = Int_table.create 32;
    externals = [];
    changed_bounds = Int_set.empty;
    pending_feasibility = Work_set.create ();
    forms = Hashtbl.create 32;
    atoms = Int_table.create 64;
    implications = Int_table.create 64;
    pending_variables = [];
    trail = [];
    trail_length = 0;
    scopes = [];
    inconsistent = None;
    equality_witness = None;
    equality_witness_status = Unchecked
  }

let allocate t variable =
  if t.count = Array.length t.variables
  then begin
    let variables = Array.make (max 16 (2 * t.count)) variable in
    Array.blit t.variables 0 variables 0 t.count;
    t.variables <- variables
  end;
  let index = t.count in
  t.variables.(index) <- variable;
  t.count <- index + 1;
  index

let external_variable t id =
  match Int_table.find_opt t.external_variables id with
  | Some index -> index
  | None ->
    let index = allocate t (variable [id, Z.one]) in
    Int_table.add t.external_variables id index;
    t.externals <- (id, index) :: t.externals;
    index

let map_add_coefficient ~poll index coefficient row =
  poll ();
  let coefficient =
    match Int_map.find_opt index row with
    | None -> coefficient
    | Some old -> Q.add ~poll old coefficient
  in
  if Q.is_zero coefficient
  then Int_map.remove index row
  else Int_map.add index coefficient row
let add_scaled = Row.add_scaled

let schedule_propagation t index =
  let variable = t.variables.(index) in
  if not variable.propagation_pending then begin
    variable.propagation_pending <- true;
    t.pending_variables <- index :: t.pending_variables
  end

let invalidate_interval t index =
  t.variables.(index).intervals <- None;
  schedule_propagation t index

let set_row t index row =
  Work_set.add index t.pending_feasibility;
  let variable = t.variables.(index) in
  invalidate_interval t index;
  let before = Option.value variable.row ~default:Row.empty in
  let after = Option.value row ~default:Row.empty in
  Row.iter_difference
    ~removed:(fun column -> Column.remove index t.variables.(column).column)
    ~added:(fun column -> Column.add index t.variables.(column).column)
    before after;
  variable.missing_lower <- -1;
  variable.missing_upper <- -1;
  variable.row <- row

let no_poll () = ()

let normalize ~poll terms boundary =
  let terms =
    List.fold_left
      (fun terms (variable, coefficient) ->
        poll ();
        let coefficient =
          match Int_map.find_opt variable terms with
          | None -> coefficient
          | Some old -> Z.add ~poll old coefficient
        in
        if Z.is_zero coefficient
        then Int_map.remove variable terms
        else Int_map.add variable coefficient terms)
      Int_map.empty terms
    |> Int_map.bindings
  in
  let divisor =
    List.fold_left (fun g (_, c) -> poll (); Z.gcd ~poll g c) Z.zero terms
  in
  let terms, boundary =
    if Z.is_zero divisor || Z.equal divisor Z.one
    then terms, boundary
    else
      ( List.map (fun (v, c) -> v, fst (Z.quot_rem ~poll c divisor)) terms,
        Z.floor_div ~poll boundary divisor )
  in
  match terms with
  | (_, first) :: _ when Z.sign first < 0 ->
    List.map (fun (v, c) -> v, Z.neg c) terms, Z.neg boundary, false
  | _ -> terms, boundary, true

let register ?(poll = no_poll) t ~literal terms boundary =
  if literal = 0 || literal = min_int
  then invalid_arg "Vox_smt_arithmetic.register: literal";
  if Int_table.mem t.atoms (abs literal)
  then invalid_arg "Vox_smt_arithmetic.register: duplicate literal";
  let terms, boundary, upper = normalize ~poll terms boundary in
  let index =
    match Hashtbl.find_opt t.forms terms with
    | Some index -> index
    | None ->
      let row =
        List.fold_left
          (fun row (id, coefficient) ->
            poll ();
            let index = external_variable t id in
            let coefficient = Q.of_z coefficient in
            match t.variables.(index).row with
            | None -> map_add_coefficient ~poll index coefficient row
            | Some source -> Row.fold (fun index value row ->
              map_add_coefficient ~poll index
                (Q.mul ~poll coefficient value) row) source row)
          Int_map.empty terms
        |> Row.of_map ~poll
      in
      let value =
        Row.fold
          (fun index coefficient value ->
            poll ();
            Q.add ~poll value
              (Q.mul ~poll coefficient t.variables.(index).value))
          row Q.zero
      in
      let index = allocate t (variable terms) in
      t.variables.(index).value <- value;
      set_row t index (Some row);
      Hashtbl.add t.forms terms index;
      index
  in
  let atom =
    { literal;
      variable = index;
      upper;
      boundary = Q.of_z boundary;
      assignment = 0
    }
  in
  Int_table.add t.atoms (abs literal) atom;
  t.variables.(index).atoms <- atom :: t.variables.(index).atoms;
  schedule_propagation t index

let record t change =
  t.trail <- change :: t.trail;
  t.trail_length <- t.trail_length + 1

let invalidate_bounds t index =
  Work_set.add index t.pending_feasibility;
  let variable = t.variables.(index) in
  invalidate_interval t index;
  Column.iter (invalidate_interval t) variable.column

let invalidate_equality_witness t variable lower upper =
  if t.equality_witness_status <> Unchecked then begin
    let fixed lower upper = match lower, upper with
      | Some lower, Some upper when Q.equal lower.number upper.number ->
        Some lower.number
      | _ -> None in
    if not (Option.equal Q.equal
        (fixed variable.lower variable.upper) (fixed lower upper))
    then t.equality_witness_status <- Unchecked
  end

let assert_literal t literal =
  if literal = 0 || literal = min_int
  then invalid_arg "Vox_smt_arithmetic.assert_literal: literal";
  let atom =
    match Int_table.find_opt t.atoms (abs literal) with
    | Some atom -> atom
    | None -> invalid_arg "Vox_smt_arithmetic.assert_literal: unknown literal"
  in
  if atom.assignment = literal
  then ()
  else if atom.assignment <> 0
  then begin
    record t (Inconsistent t.inconsistent);
    t.inconsistent <- Some [literal; atom.assignment]
  end
  else begin
    record t (Assignment (atom, atom.assignment));
    atom.assignment <- literal;
    Int_table.remove t.implications (abs literal);
    let positive = literal = atom.literal in
    let upper = if positive then atom.upper else not atom.upper in
    let number =
      if positive
      then atom.boundary
      else if upper
      then Q.sub atom.boundary Q.one
      else Q.add atom.boundary Q.one
    in
    let variable = t.variables.(atom.variable) in
    let old = if upper then variable.upper else variable.lower in
    let improves =
      match old with
      | None -> true
      | Some old ->
        let comparison = Q.compare number old.number in
        if upper then comparison < 0 else comparison > 0
    in
    if improves
    then begin
      record t (Bounds (atom.variable, variable.lower, variable.upper));
      t.changed_bounds <- Int_set.add atom.variable t.changed_bounds;
      invalidate_bounds t atom.variable;
      let bound = Some { number; reasons = [literal] } in
      if upper then begin
        invalidate_equality_witness t variable variable.lower bound;
        variable.upper <- bound
      end else begin
        invalidate_equality_witness t variable bound variable.upper;
        variable.lower <- bound
      end
    end
  end

let level t = List.length t.scopes
let push t = t.scopes <- t.trail_length :: t.scopes

let rec undo_to t stop =
  if t.trail_length > stop
  then
    match t.trail with
    | [] -> invalid_arg "Vox_smt_arithmetic: corrupt trail"
    | change :: trail ->
      (match change with
      | Bounds (index, lower, upper) ->
        let variable = t.variables.(index) in
        t.changed_bounds <- Int_set.add index t.changed_bounds;
        invalidate_bounds t index;
        invalidate_equality_witness t variable lower upper;
        variable.lower <- lower;
        variable.upper <- upper
      | Assignment (atom, assignment) ->
        atom.assignment <- assignment;
        if assignment = 0 then
          schedule_propagation t atom.variable
        else Int_table.remove t.implications (abs atom.literal)
      | Inconsistent previous -> t.inconsistent <- previous);
      t.trail <- trail;
      t.trail_length <- t.trail_length - 1;
      undo_to t stop

let pop t =
  match t.scopes with
  | [] -> invalid_arg "Vox_smt_arithmetic.pop"
  | stop :: scopes ->
    undo_to t stop;
    t.scopes <- scopes

let backtrack t target =
  if target < 0 || target > level t
  then invalid_arg "Vox_smt_arithmetic.backtrack";
  while level t > target do
    pop t
  done

let reset t =
  undo_to t 0;
  t.scopes <- []

let update_nonbasic ~poll t index number =
  let variable = t.variables.(index) in
  let difference = Q.sub ~poll number variable.value in
  let updates =
    Column.fold
      (fun basic updates ->
        poll ();
        let row =
          match t.variables.(basic).row with
          | Some row -> row
          | None -> invalid_arg "Vox_smt_arithmetic: nonbasic column member"
        in
        let coefficient = Row.find index row in
        let value =
          Q.add ~poll t.variables.(basic).value
            (Q.mul ~poll coefficient difference)
        in
        (basic, value) :: updates)
      variable.column []
  in
  List.iter (fun (index, value) ->
    Work_set.add index t.pending_feasibility;
    t.variables.(index).value <- value) updates;
  variable.value <- number

let pivot ~poll t leaving entering =
  let row =
    match t.variables.(leaving).row with
    | Some row -> row
    | None -> invalid_arg "Vox_smt_arithmetic: nonbasic leaving variable"
  in
  let coefficient = Row.find entering row in
  let inverse = Q.div ~poll Q.one coefficient in
  let replacement = Row.pivot ~poll row ~leaving ~entering ~inverse in
  let updates =
    Column.fold
      (fun basic updates ->
        poll ();
        if basic = leaving
        then updates
        else
          let row =
            match t.variables.(basic).row with
            | Some row -> row
            | None -> invalid_arg "Vox_smt_arithmetic: nonbasic column member"
          in
          let coefficient = Row.find entering row in
          let row =
            add_scaled ~poll (Row.remove entering row) coefficient
              replacement
          in
          (basic, row) :: updates)
      t.variables.(entering).column []
  in
  set_row t leaving None;
  List.iter (fun (index, row) -> set_row t index (Some row)) updates;
  set_row t entering (Some replacement)

let reasons reasons =
  List.sort_uniq Int.compare reasons |> Array.of_list

exception Stopped
exception Infeasible of int list
exception Entering of int * Q.t
exception Unbounded
exception Underdetermined

let check_integer_width ~poll t =
  if Int_table.length t.external_variables = 2 && t.count <= 64 then begin
    let exception Limit in
    let remaining = ref 10000 in
    let spend () =
      poll ();
      decr remaining;
      if !remaining < 0 then raise Limit in
    let rows = ref [] in
    (try
      for index = 0 to t.count - 1 do
        spend ();
        let variable = t.variables.(index) in
        match variable.original, variable.lower, variable.upper with
        | [(x, a); (y, b)], Some lower, Some upper ->
          rows := (x, y, a, b, lower, upper) :: !rows
        | _ -> ()
      done;
      let rec pairs = function
        | [] -> ()
        | (x, y, a, b, lower, upper) :: rest ->
          List.iter (fun (u, v, c, d, other_lower, other_upper) ->
            spend ();
            if x = u && y = v then begin
              let determinant = Z.sub ~poll:spend
                  (Z.mul ~poll:spend a d) (Z.mul ~poll:spend b c) in
              if not (Z.is_zero determinant) then begin
                let test p q =
                  spend ();
                  let first = Q.make ~poll:spend
                      (Z.sub ~poll:spend (Z.mul ~poll:spend p d)
                         (Z.mul ~poll:spend q c)) determinant in
                  let second = Q.make ~poll:spend
                      (Z.sub ~poll:spend (Z.mul ~poll:spend a q)
                         (Z.mul ~poll:spend b p)) determinant in
                  let interval coefficient lower upper =
                    let left, right = if Q.sign coefficient < 0
                      then upper, lower else lower, upper in
                    Q.mul ~poll:spend coefficient left.number,
                    Q.mul ~poll:spend coefficient right.number in
                  let lo, hi = interval first lower upper in
                  let other_lo, other_hi =
                    interval second other_lower other_upper in
                  let lo = Q.ceil ~poll:spend
                      (Q.add ~poll:spend lo other_lo) in
                  let hi = Q.floor ~poll:spend
                      (Q.add ~poll:spend hi other_hi) in
                  if Z.compare lo hi > 0 then
                    raise (Infeasible
                      (lower.reasons @ upper.reasons
                       @ other_lower.reasons @ other_upper.reasons)) in
                let convergents a b =
                  let rec loop numerator denominator p0 p1 q0 q1 =
                    spend ();
                    if not (Z.is_zero denominator) then begin
                      let quotient, remainder =
                        Z.quot_rem ~poll:spend numerator denominator in
                      let p = Z.add ~poll:spend p0
                          (Z.mul ~poll:spend quotient p1) in
                      let q = Z.add ~poll:spend q0
                          (Z.mul ~poll:spend quotient q1) in
                      test (if Z.sign a < 0 then Z.neg p else p)
                        (if Z.sign b < 0 then Z.neg q else q);
                      loop denominator remainder p1 p q1 q
                    end in
                  loop (Z.abs a) (Z.abs b) Z.zero Z.one Z.one Z.zero in
                convergents a b;
                convergents c d
              end
            end) rest;
          pairs rest in
      pairs !rows
    with Limit -> ())
  end

let lattice_model ~poll t =
  let exception No_model in
  let count = Int_table.length t.external_variables in
  if count = 0 || count > 16 || t.count > 2048 then false else
  let externals = Array.of_list t.externals in
  let fixed = ref [] in
  for index = 0 to t.count - 1 do
    poll ();
    let variable = t.variables.(index) in
    match variable.lower, variable.upper with
    | Some lower, Some upper when Q.equal lower.number upper.number ->
      fixed := (variable.original, lower.number) :: !fixed
    | _ -> ()
  done;
  if !fixed = [] then false else
  let positions = Int_table.create count in
  Array.iteri (fun position (id, _) ->
    poll (); Int_table.add positions id position) externals;
  let offset = Array.make count Z.zero in
  let basis = Array.init count (fun row ->
    Array.init count (fun column -> if row = column then Z.one else Z.zero)) in
  let coordinates = ref (Array.map (fun (_, index) ->
    t.variables.(index).value) externals) in
  let dimension = ref count in
  let extended_gcd a b =
    let rec loop a b sa ta sb tb =
      poll ();
      if Z.is_zero b then a, sa, ta else
      let quotient, remainder = Z.quot_rem ~poll a b in
      loop b remainder sb tb
        (Z.sub ~poll sa (Z.mul ~poll quotient sb))
        (Z.sub ~poll ta (Z.mul ~poll quotient tb)) in
    let divisor, left, right = loop (Z.abs a) (Z.abs b)
        Z.one Z.zero Z.zero Z.one in
    divisor, (if Z.sign a < 0 then Z.neg left else left),
      (if Z.sign b < 0 then Z.neg right else right) in
  let transform terms =
    let constant = ref Z.zero and coefficients = Array.make !dimension Z.zero in
    List.iter (fun (id, coefficient) ->
      poll ();
      let position = Int_table.find positions id in
      constant := Z.add ~poll !constant
          (Z.mul ~poll coefficient offset.(position));
      for column = 0 to !dimension - 1 do
        poll ();
        coefficients.(column) <- Z.add ~poll coefficients.(column)
            (Z.mul ~poll coefficient basis.(position).(column))
      done) terms;
    !constant, coefficients in
  let constrain (terms, target) =
    poll ();
    if not (Q.is_integer target) then raise No_model;
    let constant, coefficients = transform terms in
    let target = Z.sub ~poll (Q.numerator target) constant in
    if !dimension = 0 then begin
      if not (Z.is_zero target) then raise No_model
    end else begin
      for column = 1 to !dimension - 1 do
        poll ();
        let a = coefficients.(0) and b = coefficients.(column) in
        if not (Z.is_zero b) then begin
          let divisor, s, u = extended_gcd a b in
          let negative_b = Z.neg (fst (Z.quot_rem ~poll b divisor))
          and positive_a = fst (Z.quot_rem ~poll a divisor) in
          for row = 0 to count - 1 do
            poll ();
            let left = basis.(row).(0) and right = basis.(row).(column) in
            basis.(row).(0) <- Z.add ~poll
                (Z.mul ~poll s left) (Z.mul ~poll u right);
            basis.(row).(column) <- Z.add ~poll
                (Z.mul ~poll negative_b left) (Z.mul ~poll positive_a right)
          done;
          let left = !coordinates.(0) and right = !coordinates.(column) in
          !coordinates.(0) <- Q.sub ~poll
              (Q.mul ~poll (Q.of_z positive_a) left)
              (Q.mul ~poll (Q.of_z negative_b) right);
          !coordinates.(column) <- Q.add ~poll
              (Q.mul ~poll (Q.of_z (Z.neg u)) left)
              (Q.mul ~poll (Q.of_z s) right);
          coefficients.(0) <- divisor;
          coefficients.(column) <- Z.zero
        end
      done;
      let divisor = coefficients.(0) in
      if Z.is_zero divisor then begin
        if not (Z.is_zero target) then raise No_model
      end else begin
        let coordinate, remainder = Z.quot_rem ~poll target divisor in
        if not (Z.is_zero remainder) then raise No_model;
        for row = 0 to count - 1 do
          poll ();
          offset.(row) <- Z.add ~poll offset.(row)
              (Z.mul ~poll basis.(row).(0) coordinate);
          basis.(row) <- Array.sub basis.(row) 1 (!dimension - 1)
        done;
        coordinates := Array.sub !coordinates 1 (!dimension - 1);
        decr dimension
      end
    end in
  try
    List.iter constrain !fixed;
    let parameters = Array.map (Q.floor ~poll) !coordinates in
    if !dimension = 1 then begin
      let lower = ref None and upper = ref None in
      let add_lower value = lower := Some (match !lower with
        | None -> value | Some previous ->
          if Z.compare value previous > 0 then value else previous) in
      let add_upper value = upper := Some (match !upper with
        | None -> value | Some previous ->
          if Z.compare value previous < 0 then value else previous) in
      for index = 0 to t.count - 1 do
        poll ();
        let variable = t.variables.(index) in
        if Option.is_some variable.lower || Option.is_some variable.upper
        then begin
          let constant, coefficients = transform variable.original in
          let coefficient = coefficients.(0) in
          let bound upper number =
            poll ();
            let difference = Q.sub ~poll number (Q.of_z constant) in
            if Z.is_zero coefficient then begin
              if (upper && Q.sign difference < 0)
                  || (not upper && Q.sign difference > 0)
              then raise No_model
            end else begin
              let threshold = Q.div ~poll difference (Q.of_z coefficient) in
              if upper = (Z.sign coefficient > 0)
              then add_upper (Q.floor ~poll threshold)
              else add_lower (Q.ceil ~poll threshold)
            end in
          Option.iter (fun source -> bound false source.number) variable.lower;
          Option.iter (fun source -> bound true source.number) variable.upper
        end
      done;
      Option.iter (fun value ->
        if Z.compare parameters.(0) value < 0 then parameters.(0) <- value)
        !lower;
      Option.iter (fun value ->
        if Z.compare parameters.(0) value > 0 then parameters.(0) <- value)
        !upper
    end;
    let values = Array.init count (fun row ->
      let value = ref offset.(row) in
      for column = 0 to !dimension - 1 do
        poll ();
        value := Z.add ~poll !value
            (Z.mul ~poll basis.(row).(column) parameters.(column))
      done;
      !value) in
    let candidates = Array.init t.count (fun index ->
      poll ();
      let variable = t.variables.(index) in
      let value = List.fold_left (fun value (id, coefficient) ->
        poll ();
        Z.add ~poll value
          (Z.mul ~poll coefficient values.(Int_table.find positions id)))
          Z.zero variable.original |> Q.of_z in
      Option.iter (fun source ->
        if Q.compare ~poll value source.number < 0 then raise No_model)
        variable.lower;
      Option.iter (fun source ->
        if Q.compare ~poll value source.number > 0 then raise No_model)
        variable.upper;
      value) in
    for index = 0 to t.count - 1 do
      poll ();
      Option.iter (fun row ->
        let actual = Row.fold (fun column coefficient value ->
          poll ();
          Q.add ~poll value (Q.mul ~poll coefficient candidates.(column)))
            row Q.zero in
        if not (Q.equal actual candidates.(index)) then raise No_model)
        t.variables.(index).row
    done;
    Array.iteri (fun index value -> t.variables.(index).value <- value)
      candidates;
    true
  with No_model -> false

let suggest_integer_model ~poll t =
  if t.count > 256 then () else
  let count = List.length t.externals in
  if count = 0 || count > 32 then () else
  let exception Skip in
  try
    let externals = Array.of_list t.externals in
    let positions = Int_table.create count in
    Array.iteri (fun position (id, _) ->
      Int_table.add positions id position) externals;
    let bounded number =
      match Z.to_int_opt number with
      | Some n when n >= -100000000 && n <= 100000000 -> n
      | _ -> raise Skip in
    let values = Array.map (fun (_, index) ->
      poll ();
      let value = t.variables.(index).value in
      let lower = Q.floor ~poll value and upper = Q.ceil ~poll value in
      let nearest = if Q.compare ~poll
          (Q.sub ~poll value (Q.of_z lower))
          (Q.sub ~poll (Q.of_z upper) value) <= 0 then lower else upper in
      float_of_int (bounded nearest)) externals in
    let rows = ref [] in
    for index = 0 to t.count - 1 do
      poll ();
      let variable = t.variables.(index) in
      if variable.lower <> None || variable.upper <> None then begin
        let coefficients = Array.make count 0. in
        List.iter (fun (id, coefficient) ->
          poll ();
          coefficients.(Int_table.find positions id) <-
            float_of_int (bounded coefficient)) variable.original;
        let norm = max 1. (Array.fold_left (fun acc x -> acc +. abs_float x)
            0. coefficients) in
        let lower = Option.map (fun b ->
          float_of_int (bounded (Q.numerator b.number))) variable.lower in
        let upper = Option.map (fun b ->
          float_of_int (bounded (Q.numerator b.number))) variable.upper in
        rows := (coefficients, lower, upper, norm) :: !rows
      end
    done;
    let rows = Array.of_list !rows in
    let row_values = Array.map (fun (cs, _, _, _) ->
      let value = ref 0. in
      Array.iteri (fun i c -> value := !value +. c *. values.(i)) cs;
      !value) rows in
    let weights = Array.make (Array.length rows) 1. in
    let penalty row value =
      let _, lower, upper, norm = row in
      let below = match lower with None -> 0. | Some x -> max 0. (x -. value) in
      let above = match upper with None -> 0. | Some x -> max 0. (value -. x) in
      (below +. above) /. norm in
    let score () =
      let result = ref 0. in
      Array.iteri (fun i row ->
        poll ();
        result := !result +. weights.(i) *. penalty row row_values.(i)) rows;
      !result in
    let current = ref (score ()) in
    let proposals = ref 0 and stagnant = ref 0 in
    while !current > 0. && !proposals < 4096 && !stagnant < 8 do
      poll ();
      let best = ref !current and movement = ref None in
      Array.iteri (fun row_index (cs, lower, upper, _) ->
        if !proposals < 4096
           && penalty rows.(row_index) row_values.(row_index) > 0.
        then begin
          let target = match lower, upper with
            | Some x, _ when row_values.(row_index) < x -> x
            | _, Some x -> x | _ -> raise Skip in
          Array.iteri (fun position coefficient ->
            if coefficient <> 0. && !proposals < 4096 then begin
              let jump = (target -. row_values.(row_index)) /. coefficient in
              List.iter (fun delta ->
                poll (); incr proposals;
                if delta <> 0. && abs_float delta < 100000000.
                    && abs_float (values.(position) +. delta) < 100000000.
                then begin
                  let score = ref 0. in
                  Array.iteri (fun i ((coefficients, _, _, _) as row) ->
                    poll ();
                    score := !score +. weights.(i) *. penalty row
                      (row_values.(i) +. coefficients.(position) *. delta))
                    rows;
                  if !score < !best || (!score = !best && !movement = None)
                  then begin
                    best := !score;
                    movement := Some (position, delta)
                  end
                end) [floor jump; ceil jump]
            end) cs
        end) rows;
      match !movement with
      | None ->
        incr stagnant;
        Array.iteri (fun i row ->
          poll ();
          if penalty row row_values.(i) > 0.
          then weights.(i) <- weights.(i) +. 1.) rows;
        current := score ()
      | Some (position, delta) ->
        if !best >= !current then incr stagnant;
        values.(position) <- values.(position) +. delta;
        Array.iteri (fun i (cs, _, _, _) ->
          row_values.(i) <- row_values.(i) +. cs.(position) *. delta) rows;
        current := !best
    done;
    if !current = 0. then begin
      let exact = Array.map (fun x -> Z.of_int (int_of_float x)) values in
      let candidate = Array.init t.count (fun index ->
        poll ();
        let variable = t.variables.(index) in
        let value = Q.of_z (List.fold_left (fun total (id, coefficient) ->
          poll ();
          Z.add ~poll total
            (Z.mul ~poll coefficient exact.(Int_table.find positions id)))
            Z.zero variable.original) in
        (match variable.lower, variable.upper with
        | Some b, _ when Q.compare ~poll value b.number < 0 -> raise Skip
        | _, Some b when Q.compare ~poll value b.number > 0 -> raise Skip
        | _ -> ());
        value) in
      Array.iteri (fun index value ->
        poll ();
        match t.variables.(index).row with
        | None -> ()
        | Some row ->
          let derived = Row.fold (fun variable coefficient value ->
            poll ();
            Q.add ~poll value (Q.mul ~poll coefficient candidate.(variable)))
              row Q.zero in
          if not (Q.equal value derived) then raise Skip) candidate;
      Array.iteri (fun index value -> t.variables.(index).value <- value)
        candidate
    end
  with Skip -> ()

let cone_integer_model ~poll ~relax t =
  if t.count > 256 || List.length t.externals > 32 then false else
  let exception No_model in
  try
    let proposed = create () in
    for index = 0 to t.count - 1 do
      poll ();
      let source = t.variables.(index) in
      let copy = variable source.original in
      copy.value <- source.value;
      copy.lower <- Option.map (fun bound ->
        { number = (if Q.sign bound.number < 0 then Q.zero
            else bound.number); reasons = [] }) source.lower;
      copy.upper <- Option.map (fun bound ->
        { number = (if Q.sign bound.number > 0 then Q.zero
            else bound.number); reasons = [] }) source.upper;
      ignore (allocate proposed copy)
    done;
    proposed.externals <- t.externals;
    List.iter (fun (id, index) ->
      poll (); Int_table.add proposed.external_variables id index) t.externals;
    for index = 0 to t.count - 1 do
      poll ();
      Option.iter (fun row -> set_row proposed index (Some row))
        t.variables.(index).row;
      proposed.changed_bounds <- Int_set.add index proposed.changed_bounds
    done;
    if not (relax ~poll proposed) then raise No_model;
    let value id = match Int_table.find_opt proposed.external_variables id with
      | None -> Q.zero
      | Some index -> proposed.variables.(index).value in
    let scale = List.fold_left (fun scale (id, _) ->
      poll ();
      let denominator = Q.denominator (value id) in
      if Z.equal denominator Z.one || Z.equal denominator scale then scale
      else Z.mul ~poll denominator (fst (Z.quot_rem ~poll scale
          (Z.gcd ~poll scale denominator)))) Z.one t.externals in
    let scale = Q.of_z scale in
    let candidates = Array.init t.count (fun index ->
      poll ();
      let variable = t.variables.(index) in
      let actual = List.fold_left (fun actual (id, c) ->
        poll ();
        Q.add ~poll actual (Q.mul ~poll (Q.of_z c) (value id)))
          Q.zero variable.original in
      let value = Q.mul ~poll scale actual in
      if not (Q.is_integer value) then raise No_model;
      (match variable.lower, variable.upper with
      | Some bound, _ when Q.compare ~poll value bound.number < 0 ->
        raise No_model
      | _, Some bound when Q.compare ~poll value bound.number > 0 ->
        raise No_model
      | _ -> ());
      value) in
    Array.iteri (fun index value ->
      poll ();
      Option.iter (fun row ->
        let actual = Row.fold (fun column coefficient actual ->
          poll ();
          Q.add ~poll actual
            (Q.mul ~poll coefficient candidates.(column))) row Q.zero in
        if not (Q.equal value actual) then raise No_model)
        t.variables.(index).row) candidates;
    Array.iteri (fun index value -> t.variables.(index).value <- value)
      candidates;
    true
  with No_model -> false

let integer_model_proposal ~poll ~relax t =
  let exception Cutoff in
  let try_model limit proposal =
    let left = ref limit in
    let poll () =
      poll ();
      if !left = 0 then raise Cutoff;
      decr left in
    try proposal ~poll t with Cutoff -> false in
  if not (try_model 8388608 (cone_integer_model ~relax))
      && not (try_model 4096 lattice_model) then
    ignore (try_model
      262144
      (fun ~poll t -> suggest_integer_model ~poll t; true))

let construct_equality_witness ~poll t =
  let exception No_witness in
  let remaining = ref 32768 in
  let poll () =
    poll ();
    decr remaining;
    if !remaining < 0 then raise No_witness in
  let pivots = ref [] in
  let add row id coefficient =
    let coefficient = match Int_map.find_opt id row with
      | None -> coefficient
      | Some previous -> Z.add ~poll previous coefficient in
    if Z.is_zero coefficient then Int_map.remove id row
    else Int_map.add id coefficient row in
  try
    for index = 0 to t.count - 1 do
      poll ();
      let variable = t.variables.(index) in
      match variable.lower, variable.upper with
      | Some lower, Some upper when Q.equal lower.number upper.number ->
        if not (Q.is_integer lower.number) then raise No_witness;
        let row = ref (List.fold_left (fun row (id, coefficient) ->
          poll (); add row id coefficient) Int_map.empty variable.original) in
        let target = ref (Q.numerator lower.number) in
        List.iter (fun (id, source, number) ->
          poll ();
          match Int_map.find_opt id !row with
          | None -> ()
          | Some coefficient ->
            row := Int_map.remove id !row;
            target := Z.sub ~poll !target (Z.mul ~poll coefficient number);
            Int_map.iter (fun id value ->
              poll ();
              row := add !row id (Z.neg (Z.mul ~poll coefficient value)))
              source) (List.rev !pivots);
        if Int_map.is_empty !row then begin
          if not (Z.is_zero !target) then raise No_witness
        end else begin
          let pivot = Int_map.fold (fun id coefficient selected ->
            poll ();
            match selected with
            | Some _ -> selected
            | None -> if Z.equal (Z.abs coefficient) Z.one
                then Some (id, coefficient) else None) !row None in
          match pivot with
          | None -> raise No_witness
          | Some (id, coefficient) ->
            let row = Int_map.remove id !row in
            let row, target = if Z.sign coefficient > 0 then row, !target
              else Int_map.map (fun value -> poll (); Z.neg value) row,
                   Z.neg !target in
            pivots := (id, row, target) :: !pivots
        end
      | _ -> ()
    done;
    let values = Array.make t.count Z.zero in
    List.iter (fun (id, row, target) ->
      poll ();
      let value = Int_map.fold (fun id coefficient total ->
        poll ();
        let value = values.(Int_table.find t.external_variables id) in
        Z.sub ~poll total (Z.mul ~poll coefficient value)) row target in
      values.(Int_table.find t.external_variables id) <- value) !pivots;
    Some values
  with No_witness -> None

(* Tableau variables denote immutable integer linear forms. An integer
   witness for all fixed forms rules out divisibility conflicts across
   basis changes. *)
let has_equality_witness ~poll t =
  match t.equality_witness_status with
  | Valid -> true
  | Missing -> false
  | Unchecked ->
    let valid values =
      let exception Invalid in
      try
        for index = 0 to t.count - 1 do
          poll ();
          let variable = t.variables.(index) in
          match variable.lower, variable.upper with
          | Some lower, Some upper when Q.equal lower.number upper.number ->
            let value = List.fold_left (fun total (id, coefficient) ->
              poll ();
              let index = Int_table.find t.external_variables id in
              let value = if index < Array.length values
                then values.(index) else Z.zero in
              Z.add ~poll total (Z.mul ~poll coefficient value))
                Z.zero variable.original in
            if not (Q.equal (Q.of_z value) lower.number) then raise Invalid
          | _ -> ()
        done;
        true
      with Invalid -> false in
    match t.equality_witness with
    | Some values when valid values ->
      t.equality_witness_status <- Valid; true
    | _ ->
      match construct_equality_witness ~poll t with
      | Some values when valid values ->
        t.equality_witness <- Some values;
        t.equality_witness_status <- Valid; true
      | _ -> t.equality_witness_status <- Missing; false

(* Integer row operations preserve the integrality of each basis form.
   Gram-Schmidt coefficients only guide those row operations. *)
let lattice_rows ~poll ~refine t =
  if t.count > 128 then [] else
  let n = List.length t.externals in
  if n < 2 || n > 32 then [] else
  let exception Dependent in
  try
    let columns = Array.make t.count (-1) and count = ref 0 in
    for index = 0 to t.count - 1 do
      poll ();
      if Option.is_none t.variables.(index).row then begin
        columns.(index) <- !count;
        incr count
      end
    done;
    let basis = Array.of_list (List.map (fun (_, index) ->
      poll ();
      let vector = Array.make !count Q.zero in
      (match t.variables.(index).row with
      | None -> vector.(columns.(index)) <- Q.one
      | Some row -> Row.iter (fun index coefficient ->
          poll ();
          vector.(columns.(index)) <- coefficient) row);
      vector) t.externals) in
    let dot a b =
      let value = ref Q.zero in
      for column = 0 to !count - 1 do
        poll ();
        value := Q.add ~poll !value
            (Q.mul ~poll a.(column) b.(column))
      done;
      !value in
    let orthogonal = Array.map Array.copy basis in
    let mu = Array.init n (fun row -> Array.make row Q.zero) in
    let norms = Array.make n Q.zero in
    for row = 0 to n - 1 do
      poll ();
      for previous = 0 to row - 1 do
        let coefficient = Q.div ~poll
            (dot basis.(row) orthogonal.(previous)) norms.(previous) in
        mu.(row).(previous) <- coefficient;
        for column = 0 to !count - 1 do
          poll ();
          orthogonal.(row).(column) <- Q.sub ~poll
              orthogonal.(row).(column)
              (Q.mul ~poll coefficient orthogonal.(previous).(column))
        done
      done;
      let norm = dot orthogonal.(row) orthogonal.(row) in
      if Q.is_zero norm then raise Dependent;
      norms.(row) <- norm
    done;
    let half = Q.make ~poll Z.one (Z.of_int 2) in
    let delta = if refine then
        Q.make ~poll (Z.of_int 99) (Z.of_int 100)
      else Q.make ~poll (Z.of_int 3) (Z.of_int 4) in
    let row = ref 1 and steps = ref 0 in
    while !row < n && !steps < 2048 do
      poll ();
      incr steps;
      let k = !row in
      for previous = k - 1 downto 0 do
        poll ();
        let multiple = Q.floor ~poll
            (Q.add ~poll mu.(k).(previous) half) in
        if not (Z.is_zero multiple) then begin
          let multiple = Q.of_z multiple in
          for column = 0 to !count - 1 do
            poll ();
            basis.(k).(column) <- Q.sub ~poll basis.(k).(column)
                (Q.mul ~poll multiple basis.(previous).(column))
          done;
          for column = 0 to previous - 1 do
            poll ();
            mu.(k).(column) <- Q.sub ~poll mu.(k).(column)
                (Q.mul ~poll multiple mu.(previous).(column))
          done;
          mu.(k).(previous) <- Q.sub ~poll mu.(k).(previous) multiple
        end
      done;
      let old = mu.(k).(k - 1) in
      let square = Q.mul ~poll old old in
      if Q.compare ~poll norms.(k)
          (Q.mul ~poll (Q.sub ~poll delta square) norms.(k - 1)) >= 0
      then incr row
      else begin
        let norm = Q.add ~poll norms.(k)
            (Q.mul ~poll square norms.(k - 1)) in
        let coefficient = Q.div ~poll
            (Q.mul ~poll old norms.(k - 1)) norm in
        norms.(k) <- Q.div ~poll
            (Q.mul ~poll norms.(k) norms.(k - 1)) norm;
        norms.(k - 1) <- norm;
        let vector = basis.(k) in
        basis.(k) <- basis.(k - 1);
        basis.(k - 1) <- vector;
        for column = 0 to k - 2 do
          poll ();
          let value = mu.(k).(column) in
          mu.(k).(column) <- mu.(k - 1).(column);
          mu.(k - 1).(column) <- value
        done;
        mu.(k).(k - 1) <- coefficient;
        for following = k + 1 to n - 1 do
          poll ();
          let value = mu.(following).(k) in
          mu.(following).(k) <- Q.sub ~poll mu.(following).(k - 1)
              (Q.mul ~poll old value);
          mu.(following).(k - 1) <- Q.add ~poll value
              (Q.mul ~poll coefficient mu.(following).(k))
        done;
        row := max 1 (k - 1)
      end
    done;
    Array.to_list (Array.map (fun vector ->
      let coefficients = ref Int_map.empty in
      for index = 0 to t.count - 1 do
        poll ();
        let column = columns.(index) in
        if column >= 0 && not (Q.is_zero vector.(column)) then
          coefficients := Int_map.add index vector.(column) !coefficients
      done;
      Row.of_map ~poll !coefficients) basis)
  with Dependent -> []

let rec check_lazy ?(cut_basis = Tableau) ?(split_forms = false)
    ?(optimize = false)
    ?(model_search = true) ?(phase = Final)
    ?(cuts = false) ?budget
    ?(cancelled = fun () -> false)
    ?(charge = fun () -> true) t =
  Option.iter
    (fun budget ->
      if budget < 0 then invalid_arg "Vox_smt_arithmetic.check: budget")
    budget;
  let remaining = ref budget and ticks = ref 0 in
  let poll () =
    if not (charge ()) then raise Stopped;
    incr ticks;
    (match !remaining with
    | None -> ()
    | Some 0 -> raise Stopped
    | Some count -> remaining := Some (count - 1));
    if (!ticks = 1 || !ticks land 255 = 0) && cancelled () then raise Stopped
  in
  let compare = Q.compare ~poll in
  let violates (variable : variable) =
    match variable.lower, variable.upper with
    | Some bound, _ when compare variable.value bound.number < 0 ->
      Some (true, bound)
    | _, Some bound when compare variable.value bound.number > 0 ->
      Some (false, bound)
    | _ -> None
  in
  let blocked coefficient increase (variable : variable) =
    let increasing = increase = (Q.sign coefficient > 0) in
    let bound = if increasing then variable.upper else variable.lower in
    match bound with
    | None -> None
    | Some bound ->
      let c = compare variable.value bound.number in
      if (increasing && c >= 0) || ((not increasing) && c <= 0)
      then Some bound
      else None
  in
  let pivots = ref 0 in
  let rec repair () =
    incr pivots;
    poll ();
    let rec find () =
      match Work_set.min_elt_opt t.pending_feasibility with
      | None -> None
      | Some index ->
        let variable = t.variables.(index) in
        let next () =
          Work_set.remove index t.pending_feasibility;
          find () in
        match variable.row with
        | None -> next ()
        | Some row -> (
          match violates variable with
          | Some violation -> Some (index, row, violation)
          | None -> next ())
    in
    match find () with
    | None -> ()
    | Some (leaving, row, (increase, target)) ->
      let explanation = ref target.reasons in
      let candidate =
        let best = ref None and smallest = ref max_int in
        (try
          Row.iter
            (fun index coefficient ->
              poll ();
              match blocked coefficient increase t.variables.(index) with
              | Some bound ->
                explanation := List.rev_append bound.reasons !explanation
              | None ->
                (* Eventually use Bland's rule to prevent cycling. *)
                if !pivots > 100
                then raise (Entering (index, coefficient));
                let size = Column.cardinal t.variables.(index).column in
                if !best = None && size <= 8
                then raise (Entering (index, coefficient));
                if size < !smallest then begin
                  smallest := size;
                  best := Some (index, coefficient)
                end)
            row
        with Entering (index, coefficient) ->
          best := Some (index, coefficient));
        !best
      in
      (match candidate with
      | None -> raise (Infeasible !explanation)
      | Some (entering, coefficient) ->
        let difference =
          Q.sub ~poll target.number t.variables.(leaving).value
        in
        let number =
          Q.add ~poll t.variables.(entering).value
            (Q.div ~poll difference coefficient)
        in
        (* The value update preserves every row, including when a later poll
           interrupts before the pivot. *)
        t.changed_bounds <- Int_set.add entering t.changed_bounds;
        update_nonbasic ~poll t entering number;
        pivot ~poll t leaving entering;
        t.changed_bounds <- Int_set.remove entering t.changed_bounds;
        repair ())
  in
  let complete_basis () =
    if t.count <= 128 then
      for entering = 0 to t.count - 1 do
        poll ();
        let variable = t.variables.(entering) in
        let at_bound = function
          | None -> false
          | Some bound -> Q.equal variable.value bound.number in
        if Option.is_none variable.row && not (at_bound variable.lower)
            && not (at_bound variable.upper) then begin
          let direction positive =
            let selected = ref None in
            let consider index coefficient =
              poll ();
              let owner = t.variables.(index) in
              let increasing = positive = (Q.sign coefficient > 0) in
              let bound = if increasing then owner.upper else owner.lower in
              Option.iter (fun bound ->
                let step = Q.div ~poll
                    (Q.sub ~poll bound.number owner.value) coefficient in
                let magnitude = if positive then step else Q.neg step in
                if Q.sign magnitude >= 0 then
                  match !selected with
                  | Some (_, _, old) when compare old magnitude <= 0 -> ()
                  | _ -> selected := Some (index, step, magnitude)) bound in
            consider entering Q.one;
            Column.iter (fun index ->
              match t.variables.(index).row with
              | Some row -> consider index (Row.find entering row)
              | None -> invalid_arg
                  "Vox_smt_arithmetic: nonbasic column member")
              variable.column;
            !selected in
          let selected = match direction true with
            | Some _ as selected -> selected
            | None -> direction false in
          match selected with
          | None -> ()
          | Some (leaving, step, _) ->
            update_nonbasic ~poll t entering
              (Q.add ~poll variable.value step);
            if leaving <> entering then pivot ~poll t leaving entering
        end
      done
  in
  let minimize_basis () =
    if t.count <= 128 then begin
      let exception Finished in
      try
        for _ = 1 to 128 do
          poll ();
          let objective = Array.make t.count Q.zero in
          List.iter (fun (_, index) ->
            poll ();
            match t.variables.(index).row with
            | None -> objective.(index) <-
                Q.add ~poll objective.(index) Q.one
            | Some row -> Row.iter (fun index coefficient ->
                poll ();
                objective.(index) <-
                  Q.add ~poll objective.(index) coefficient) row)
            t.externals;
          let chosen = ref None in
          let entering = ref 0 in
          while !chosen = None && !entering < t.count do
            poll ();
            let index = !entering in
            incr entering;
            let variable = t.variables.(index) in
            let cost = objective.(index) in
            if Option.is_none variable.row && not (Q.is_zero cost) then begin
              let positive = Q.sign cost < 0 in
              let bound = if positive then variable.upper
                else variable.lower in
              let blocked = match bound with
                | Some bound -> Q.equal variable.value bound.number
                | None -> false in
              if not blocked then begin
                let selected = ref None in
                let consider owner coefficient =
                  poll ();
                  let variable = t.variables.(owner) in
                  let increasing = positive = (Q.sign coefficient > 0) in
                  let bound = if increasing then variable.upper
                    else variable.lower in
                  Option.iter (fun bound ->
                    let step = Q.div ~poll
                        (Q.sub ~poll bound.number variable.value)
                        coefficient in
                    let magnitude = if positive then step else Q.neg step in
                    if Q.sign magnitude >= 0 then
                      match !selected with
                      | Some (old_owner, _, old)
                        when compare old magnitude < 0
                          || (compare old magnitude = 0
                              && old_owner < owner) -> ()
                      | _ -> selected := Some (owner, step, magnitude))
                    bound in
                consider index Q.one;
                Column.iter (fun owner ->
                  match t.variables.(owner).row with
                  | Some row -> consider owner (Row.find index row)
                  | None -> invalid_arg
                      "Vox_smt_arithmetic: nonbasic column member")
                  variable.column;
                Option.iter (fun (leaving, step, _) ->
                  chosen := Some (index, leaving, step)) !selected
              end
            end
          done;
          match !chosen with
          | None -> raise Finished
          | Some (entering, leaving, step) ->
            update_nonbasic ~poll t entering
              (Q.add ~poll t.variables.(entering).value step);
            if leaving <> entering then pivot ~poll t leaving entering
        done
      with Finished -> ()
    end
  in
  let interval upper (variable : variable) =
    let direct = if upper then variable.upper else variable.lower in
    match variable.row with
    | None -> direct
    | Some row ->
      let owner = variable in
      let missing =
        if upper then owner.missing_upper else owner.missing_lower
      in
      let unavailable =
        missing >= 0
        && (let source = t.variables.(missing lsr 1) in
            Option.is_none
              (if missing land 1 = 1 then source.upper else source.lower))
      in
      let derived =
        if unavailable then None else try
          Some
            (Row.fold
               (fun index coefficient accumulated ->
                 poll ();
                 let variable = t.variables.(index) in
                 let use_upper = upper = (Q.sign coefficient > 0) in
                 let bound =
                   if use_upper
                   then variable.upper
                   else variable.lower
                 in
                 match bound with
                 | None ->
                   let missing = 2 * index + (if use_upper then 1 else 0) in
                   if upper then owner.missing_upper <- missing
                   else owner.missing_lower <- missing;
                   raise Unbounded
                 | Some bound ->
                  { number =
                      Q.add ~poll accumulated.number
                        (Q.mul ~poll coefficient bound.number);
                    reasons =
                      List.rev_append bound.reasons accumulated.reasons
                  })
               row { number = Q.zero; reasons = [] })
        with Unbounded -> None
      in
      let derived =
        Option.map
          (fun bound ->
            let number =
              if upper
              then Q.floor ~poll bound.number
              else Q.ceil ~poll bound.number
            in
            { bound with number = Q.of_z number })
          derived
      in
      match direct, derived with
      | None, bound | bound, None -> bound
      | Some a, Some b ->
        let c = compare a.number b.number in
        if (upper && c <= 0) || ((not upper) && c >= 0)
        then Some a
        else Some b
  in
  let gomory_row row =
    if Row.cardinal row > 32 then None else
      let fraction q = Q.sub ~poll q (Q.of_z (Q.floor ~poll q)) in
      let offset = ref Q.zero and reasons = ref [] and terms = ref [] in
      try
        Row.iter (fun index coefficient ->
          poll ();
          if not (Q.is_integer coefficient) then begin
            let variable = t.variables.(index) in
            let sign, bound = match variable.lower, variable.upper with
              | Some bound, _ when Q.equal variable.value bound.number ->
                Z.one, bound
              | _, Some bound when Q.equal variable.value bound.number ->
                Z.minus_one, bound
              | _ -> raise Unbounded in
            if not (Q.is_integer bound.number) then raise Unbounded;
            offset := Q.add ~poll !offset
                (Q.mul ~poll coefficient bound.number);
            reasons := List.rev_append bound.reasons !reasons;
            let coefficient = fraction
                (Q.neg (Q.mul ~poll (Q.of_z sign) coefficient)) in
            terms := (index, sign, bound.number, coefficient) :: !terms
          end) row;
        let boundary = fraction !offset in
        if Q.is_zero boundary then raise Unbounded;
        let terms = if not optimize then !terms else
          List.map (fun (index, sign, base, coefficient) ->
          poll ();
          let coefficient =
            if compare coefficient boundary <= 0 then coefficient
            else Q.div ~poll
              (Q.mul ~poll boundary (Q.sub ~poll Q.one coefficient))
              (Q.sub ~poll Q.one boundary) in
          index, sign, base, coefficient) !terms in
        let denominator = List.fold_left (fun denominator (_, _, _, q) ->
            let d = Q.denominator q in
            Z.mul ~poll d
              (fst (Z.quot_rem ~poll denominator
                  (Z.gcd ~poll denominator d))))
            (Q.denominator boundary) terms in
        let scale q = Q.numerator (Q.mul ~poll (Q.of_z denominator) q) in
        let boundary = ref (scale boundary) and coefficients = ref Int_map.empty
        and actual = ref Q.zero in
        List.iter (fun (index, sign, base, q) ->
          poll ();
          let coefficient = Z.mul ~poll sign (scale q) in
          boundary := Z.add ~poll !boundary
              (Z.mul ~poll coefficient (Q.numerator base));
          actual := Q.add ~poll !actual
              (Q.mul ~poll (Q.of_z coefficient) t.variables.(index).value);
          List.iter (fun (id, original) ->
            let old = Option.value (Int_map.find_opt id !coefficients)
                ~default:Z.zero in
            let value = Z.sub ~poll old (Z.mul ~poll coefficient original) in
            coefficients := if Z.is_zero value
              then Int_map.remove id !coefficients
              else Int_map.add id value !coefficients)
            t.variables.(index).original) terms;
        if compare !actual (Q.of_z !boundary) >= 0 then None
        else Some (Int_map.bindings !coefficients, Z.neg !boundary,
          Array.of_list (List.sort_uniq Int.compare !reasons))
      with Unbounded -> None
  in
  let gomory index =
    match t.variables.(index).row with
    | None -> None
    | Some row -> gomory_row row in
  let fixed (variable : variable) =
    match variable.lower, variable.upper with
    | Some lower, Some upper when Q.equal lower.number upper.number ->
      Some (lower.number, List.rev_append lower.reasons upper.reasons)
    | _ -> None
  in
  let rational_gcd left right =
    if Q.is_zero left then if Q.sign right < 0 then Q.neg right else right
    else if Q.equal left Q.one && Q.is_integer right then Q.one
    else
      let numerator = Z.gcd ~poll (Q.numerator left) (Q.numerator right) in
      let left = Q.denominator left and right = Q.denominator right in
      let common = Z.gcd ~poll left right in
      let denominator =
        Z.mul ~poll (fst (Z.quot_rem ~poll left common)) right
      in
      Q.make ~poll numerator denominator
  in
  (* Tableau variables remain integer linear forms after basis changes. *)
  let check_divisibility () =
    let fixed = Array.init t.count (fun index -> fixed t.variables.(index)) in
    let known = Array.map (Option.map fst) fixed in
    let dependencies = Array.make t.count [] in
    let explanation initial =
      let pending = ref initial and reasons = ref [] in
      let visited = Array.make t.count false in
      while !pending <> [] do
        match !pending with
        | [] -> ()
        | index :: rest ->
          poll ();
          pending := rest;
          if not visited.(index) then begin
            visited.(index) <- true;
            match fixed.(index) with
            | Some (_, source) -> reasons := List.rev_append source !reasons
            | None -> pending := List.rev_append dependencies.(index) !pending
          end
      done;
      !reasons
    in
    let pending = Queue.create () and queued = Array.make t.count false in
    let enqueue index =
      if not queued.(index)
      then begin
        queued.(index) <- true;
        Queue.add index pending
      end
    in
    for index = 0 to t.count - 1 do
      if Option.is_some t.variables.(index).row then enqueue index
    done;
    while not (Queue.is_empty pending) do
      poll ();
      let index = Queue.take pending in
      queued.(index) <- false;
      match t.variables.(index).row with
      | None -> ()
      | Some row ->
        (try
          let unknown = ref None in
          let count index coefficient =
            poll ();
            if Option.is_none known.(index)
            then
              match !unknown with
              | None -> unknown := Some (index, coefficient)
              | Some _ -> raise Underdetermined
          in
          count index (Q.neg Q.one);
          Row.iter count row;
          (match !unknown with
          | Some (target, _) ->
            let sources = ref [] in
            let accumulate index _ =
              poll ();
              if Option.is_some known.(index) then sources := index :: !sources
            in
            accumulate index Q.one;
            Row.iter accumulate row;
            (* Repair made each row feasible; known values fix this one. *)
            let value = t.variables.(target).value in
            if not (Q.is_integer value)
            then raise (Infeasible (explanation !sources));
            known.(target) <- Some value;
            dependencies.(target) <- !sources;
            Column.iter enqueue t.variables.(target).column
          | None -> ())
        with Underdetermined -> ())
    done;
    for index = 0 to t.count - 1 do
      poll ();
      let variable = t.variables.(index) in
      match variable.row with
      | None -> ()
      | Some row ->
        let witnessed =
          Q.is_integer variable.value
          && (let rec scan offset =
                offset = row.Row.length
                || (poll ();
                    Q.is_integer t.variables.(row.Row.indices.(offset)).value
                    && scan (offset + 1)) in
              scan 0)
        in
        if not witnessed then begin
          let offset, divisor =
            match known.(index) with
            | None -> ref Q.zero, ref Q.one
            | Some number -> ref (Q.neg number), ref Q.zero
          in
          Row.iter (fun index coefficient ->
              poll ();
              match known.(index) with
              | Some number ->
                offset := Q.add ~poll !offset (Q.mul ~poll coefficient number)
              | None -> divisor := rational_gcd !divisor coefficient) row;
          let possible =
            Q.is_zero !offset
            || (not (Q.is_zero !divisor)
                && Q.is_integer (Q.div ~poll !offset !divisor))
          in
          if not possible then begin
            let sources = ref [] in
            let accumulate index _ =
              poll ();
              if Option.is_some known.(index) then sources := index :: !sources
            in
            accumulate index Q.one;
            Row.iter accumulate row;
            raise (Infeasible (explanation !sources))
          end
        end
    done
  in
  try
    poll ();
    Option.iter (fun explanation -> raise (Infeasible explanation))
      t.inconsistent;
    Int_set.iter (fun index ->
      poll ();
      let variable = t.variables.(index) in
      (match variable.lower, variable.upper with
      | Some lower, Some upper when compare lower.number upper.number > 0 ->
        raise (Infeasible (List.rev_append lower.reasons upper.reasons))
      | _ -> ());
      match variable.row with
      | Some _ -> ()
      | None -> (
        match violates variable with
        | Some (_, bound) -> update_nonbasic ~poll t index bound.number
        | None -> ()))
      t.changed_bounds;
    t.changed_bounds <- Int_set.empty;
    repair ();
    if phase = Final && List.exists (fun (_, index) ->
      poll (); not (Q.is_integer t.variables.(index).value)) t.externals
    then begin
      check_integer_width ~poll t;
      if model_search then integer_model_proposal ~poll
        ~relax:(fun ~poll proposed ->
          match check_lazy ~phase:Propagation ~model_search:false
              ~charge:(fun () -> poll (); true) proposed with
          | Consistent _ -> true
          | Unknown | Conflict _ -> false) t
    end;
    if phase = Final && (cuts || split_forms)
        && optimize && List.exists (fun (_, index) ->
      poll (); not (Q.is_integer t.variables.(index).value)) t.externals
    then begin complete_basis (); minimize_basis () end;
    let model = ref [] and split = ref None and split_index = ref max_int in
    let best = ref None in
    if phase = Final then List.iter
      (fun (id, index) ->
        poll ();
        let variable = t.variables.(index) in
        model := (id, variable.value) :: !model;
        if not (Q.is_integer variable.value) then begin
          let denominator = Q.denominator variable.value in
          let comparison = match !best with
            | None -> -1 | Some old -> Z.compare denominator old in
          if comparison < 0 || (comparison = 0 && index < !split_index) then
          begin
            best := Some denominator;
            split := Some (id, Q.floor ~poll variable.value);
            split_index := index
          end
        end)
      t.externals;
    if Option.is_some !split && not (has_equality_witness ~poll t)
    then check_divisibility ();
    let lattice =
      if Option.is_some !split
          && (split_forms || (cut_basis <> Tableau && optimize && cuts))
      then lattice_rows ~poll ~refine:(cut_basis = Combined) t
      else [] in
    let cut = if not optimize then (if cuts && Option.is_some !split
      then gomory !split_index else None)
    else if cuts && Option.is_some !split then begin
      let chosen = ref None in
      let score (terms, boundary, _) =
        let actual, norm = List.fold_left (fun (actual, norm) (id, a) ->
          poll ();
          let index = Int_table.find t.external_variables id in
          Q.add ~poll actual (Q.mul ~poll (Q.of_z a)
            t.variables.(index).value),
          Z.add ~poll norm (Z.abs a)) (Q.zero, Z.zero) terms in
        let boundary = if cut_basis = Tableau then boundary else
          let divisor = List.fold_left (fun divisor (_, coefficient) ->
            poll (); Z.gcd ~poll divisor coefficient) Z.zero terms in
          if Z.is_zero divisor then boundary else
            Z.mul ~poll divisor (Z.floor_div ~poll boundary divisor) in
        let violation = Q.sub ~poll actual (Q.of_z boundary) in
        Q.div ~poll violation (Q.of_z (if Z.is_zero norm then Z.one
          else norm)) in
      let consider_cut cut =
          match cut with
          | None -> ()
          | Some cut ->
            let value = score cut in
            (match !chosen with
            | Some (_, old) when compare old value >= 0 -> ()
            | _ -> chosen := Some (cut, value)) in
      let consider index =
        poll ();
        if not (Q.is_integer t.variables.(index).value) then
          consider_cut (gomory index) in
      if cut_basis <> Tableau then
        List.iter (fun row -> poll (); consider_cut (gomory_row row))
          lattice;
      if cut_basis <> Reduced || Option.is_none !chosen then begin
        if t.count <= 128 then
          for index = 0 to t.count - 1 do consider index done
        else consider !split_index
      end;
      Option.map fst !chosen
    end else None in
    let split_form = if not split_forms || Option.is_none !split
      then None else begin
      let chosen = ref None in
      List.iter (fun row ->
        poll ();
        let coefficients = ref Int_map.empty and norm = ref Q.zero in
        Row.iter (fun index coefficient ->
          poll ();
          norm := Q.add ~poll !norm (Q.mul ~poll coefficient coefficient);
          List.iter (fun (id, original) ->
            poll ();
            let old = Option.value (Int_map.find_opt id !coefficients)
                ~default:Q.zero in
            let value = Q.add ~poll old
                (Q.mul ~poll coefficient (Q.of_z original)) in
            coefficients := if Q.is_zero value
              then Int_map.remove id !coefficients
              else Int_map.add id value !coefficients)
            t.variables.(index).original) row;
        if not (Int_map.is_empty !coefficients)
            && Int_map.for_all (fun _ q -> poll (); Q.is_integer q)
              !coefficients then begin
          let terms = List.map (fun (id, q) ->
            poll (); id, Q.numerator q) (Int_map.bindings !coefficients) in
          let divisor = List.fold_left (fun divisor (_, coefficient) ->
            poll (); Z.gcd ~poll divisor coefficient) Z.zero terms in
          let divisor = match terms with
            | (_, coefficient) :: _ when Z.sign coefficient < 0 ->
              Z.neg divisor
            | _ -> divisor in
          let terms = List.map (fun (id, coefficient) ->
            poll (); id, fst (Z.quot_rem ~poll coefficient divisor)) terms in
          let actual = List.fold_left (fun total (id, coefficient) ->
            poll ();
            let index = Int_table.find t.external_variables id in
            Q.add ~poll total (Q.mul ~poll (Q.of_z coefficient)
                t.variables.(index).value)) Q.zero terms in
          if not (Q.is_integer actual) then begin
            let bound = Q.floor ~poll actual in
            let fraction = Q.sub ~poll actual (Q.of_z bound) in
            let opposite = Q.sub ~poll Q.one fraction in
            let distance = if compare fraction opposite < 0
              then fraction else opposite in
            let divisor = Q.of_z divisor in
            let norm = Q.div ~poll !norm (Q.mul ~poll divisor divisor) in
            let score = Q.div ~poll (Q.mul ~poll distance distance) norm in
            match !chosen with
            | Some (_, old) when compare old score >= 0 -> ()
            | _ -> chosen := Some ((terms, bound), score)
          end
        end) lattice;
      Option.map fst !chosen
    end in
    let rec propagate_atoms variable = function
      | [] -> ()
      | (atom : atom) :: rest ->
        poll ();
        let id = abs atom.literal in
        if atom.assignment = 0 then begin
          let lower, upper =
            match variable.intervals with
            | Some bounds -> bounds
            | None ->
              let bounds = interval false variable, interval true variable in
              variable.intervals <- Some bounds;
              bounds
          in
          let implied =
            if atom.upper then
              match upper, lower with
              | Some bound, _ when compare bound.number atom.boundary <= 0 ->
                Some (atom.literal, bound.reasons)
              | _, Some bound when compare bound.number atom.boundary > 0 ->
                Some (-atom.literal, bound.reasons)
              | _ -> None
            else
              match lower, upper with
              | Some bound, _ when compare bound.number atom.boundary >= 0 ->
                Some (atom.literal, bound.reasons)
              | _, Some bound when compare bound.number atom.boundary < 0 ->
                Some (-atom.literal, bound.reasons)
              | _ -> None
          in
          match implied with
          | None -> Int_table.remove t.implications id
          | Some (literal, explanation) ->
            Int_table.replace t.implications id
              (literal, lazy (reasons explanation))
        end;
        propagate_atoms variable rest
    in
    let rec propagate () =
      match t.pending_variables with
      | [] -> ()
      | index :: rest ->
        poll ();
        let variable = t.variables.(index) in
        propagate_atoms variable variable.atoms;
        variable.propagation_pending <- false;
        t.pending_variables <- rest;
        propagate ()
    in
    propagate ();
    let propagations = Int_table.fold
        (fun _ implication propagations ->
          poll ();
          implication :: propagations)
        t.implications [] in

    Consistent
      { model = !model;
        split = !split;
        split_form;
        cut;
        propagations = List.sort (fun (a, _) (b, _) -> Int.compare a b)
                         propagations
      }
  with
  | Stopped -> Unknown
  | Infeasible explanation -> Conflict (reasons explanation)

let value t id =
  Option.map
    (fun index -> t.variables.(index).value)
    (Int_table.find_opt t.external_variables id)

let check ?cut_basis ?split_forms ?optimize ?model_search ?phase ?cuts
    ?budget ?cancelled ?charge t =
  match check_lazy ?cut_basis ?split_forms ?optimize ?model_search ?phase
      ?cuts ?budget ?cancelled ?charge t with
  | Conflict reason -> Conflict reason
  | Unknown -> Unknown
  | Consistent r -> Consistent
      { r with propagations = List.map (fun (lit, reason) ->
          lit, Lazy.force reason) r.propagations }

let iter_split_support t id ~f =
  f id;
  match Int_table.find_opt t.external_variables id with
  | None -> ()
  | Some index ->
    let visit index =
      List.iter (fun (id, _) -> f id) t.variables.(index).original in
    let row index =
      visit index;
      Option.iter (Row.iter (fun index _ -> visit index))
        t.variables.(index).row in
    match t.variables.(index).row with
    | Some _ -> row index
    | None -> Column.iter row t.variables.(index).column

let is_fixed t id =
  let fixed index =
    let variable = t.variables.(index) in
    match variable.lower, variable.upper with
    | Some lower, Some upper -> Q.equal lower.number upper.number
    | _ -> false in
  match Hashtbl.find_opt t.forms [id, Z.one] with
  | Some index when fixed index -> true
  | _ ->
    match Int_table.find_opt t.external_variables id with
    | Some index -> fixed index
    | None -> false

let variable_count t = Int_table.length t.external_variables
