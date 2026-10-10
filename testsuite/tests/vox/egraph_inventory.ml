(* Prints the e-graph's declaration inventory,
   verification/library/vox_egraph_rule_handle.spec.json: every top-level
   declaration of each public semantic file, with its exact text, and the
   trusted standard-library primitives the semantics use. egraph_boundary.ml
   compares the output with the committed inventory, so a change to any of
   these files fails the test until the inventory is promoted, and the
   change to the trusted surface shows in the inventory's diff. Argument:
   the repository root. *)

let files =
  [ "vox_egraph_language_spec.ml"; "vox_egraph_rule_spec.ml";
    "vox_egraph_derivation_spec.ml"; "vox_egraph_match_spec.ml";
    "vox_egraph_snapshot_spec.ml"; "vox_egraph_preservation_spec.ml";
    "vox_egraph_closure_spec.ml"; "vox_egraph_quantifier.mli";
    "vox_egraph_saturation_spec.ml"; "vox_egraph_congruence_spec.ml";
    "vox_egraph_fixedpoint_spec.ml"; "vox_egraph_interpret_wrapping.mli";
    "vox_egraph_rule_handle.mli" ]

let public_contracts =
  [ "vox_egraph_interpret_wrapping.mli"; "vox_egraph_rule_handle.mli" ]

(* The trusted primitives: file, name, first and last line. *)
let primitives =
  [ "stdlib/iarray.mli",
    [ "Iarray.length", 35, 39; "Iarray.Refined.get", 53, 62 ];
    "stdlib/stdlib.mli",
    [ "Stdlib.( = )", 129, 129; "Stdlib.( <> )", 134, 134;
      "Stdlib.( < )", 139, 139; "Stdlib.( > )", 144, 144;
      "Stdlib.( <= )", 149, 149; "Stdlib.( >= )", 154, 154;
      "Stdlib.( + )", 318, 319; "Stdlib.( - )", 324, 325 ] ]

(* SHA-256, on 32-bit words held in native integers. *)
let sha256 message =
  let k =
    [| 0x428a2f98; 0x71374491; 0xb5c0fbcf; 0xe9b5dba5; 0x3956c25b; 0x59f111f1;
       0x923f82a4; 0xab1c5ed5; 0xd807aa98; 0x12835b01; 0x243185be; 0x550c7dc3;
       0x72be5d74; 0x80deb1fe; 0x9bdc06a7; 0xc19bf174; 0xe49b69c1; 0xefbe4786;
       0x0fc19dc6; 0x240ca1cc; 0x2de92c6f; 0x4a7484aa; 0x5cb0a9dc; 0x76f988da;
       0x983e5152; 0xa831c66d; 0xb00327c8; 0xbf597fc7; 0xc6e00bf3; 0xd5a79147;
       0x06ca6351; 0x14292967; 0x27b70a85; 0x2e1b2138; 0x4d2c6dfc; 0x53380d13;
       0x650a7354; 0x766a0abb; 0x81c2c92e; 0x92722c85; 0xa2bfe8a1; 0xa81a664b;
       0xc24b8b70; 0xc76c51a3; 0xd192e819; 0xd6990624; 0xf40e3585; 0x106aa070;
       0x19a4c116; 0x1e376c08; 0x2748774c; 0x34b0bcb5; 0x391c0cb3; 0x4ed8aa4a;
       0x5b9cca4f; 0x682e6ff3; 0x748f82ee; 0x78a5636f; 0x84c87814; 0x8cc70208;
       0x90befffa; 0xa4506ceb; 0xbef9a3f7; 0xc67178f2 |]
  in
  let h =
    [| 0x6a09e667; 0xbb67ae85; 0x3c6ef372; 0xa54ff53a; 0x510e527f; 0x9b05688c;
       0x1f83d9ab; 0x5be0cd19 |]
  in
  let mask x = x land 0xffffffff in
  let rotr x n = mask ((x lsr n) lor (x lsl (32 - n))) in
  let length = String.length message in
  let padded =
    let zeros = (55 - length) land 63 in
    let buf = Buffer.create (length + zeros + 9) in
    Buffer.add_string buf message;
    Buffer.add_char buf '\x80';
    Buffer.add_string buf (String.make zeros '\x00');
    for i = 7 downto 0 do
      Buffer.add_char buf (Char.chr (((length * 8) lsr (8 * i)) land 0xff))
    done;
    Buffer.contents buf
  in
  let w = Array.make 64 0 in
  for block = 0 to (String.length padded / 64) - 1 do
    for t = 0 to 15 do
      let byte i = Char.code padded.[(block * 64) + (t * 4) + i] in
      w.(t) <- (byte 0 lsl 24) lor (byte 1 lsl 16) lor (byte 2 lsl 8) lor byte 3
    done;
    for t = 16 to 63 do
      let s0 = rotr w.(t - 15) 7 lxor rotr w.(t - 15) 18 lxor (w.(t - 15) lsr 3)
      and s1 = rotr w.(t - 2) 17 lxor rotr w.(t - 2) 19 lxor (w.(t - 2) lsr 10)
      in
      w.(t) <- mask (w.(t - 16) + s0 + w.(t - 7) + s1)
    done;
    let v = Array.copy h in
    for t = 0 to 63 do
      let e = v.(4) and a = v.(0) in
      let s1 = rotr e 6 lxor rotr e 11 lxor rotr e 25 in
      let ch = (e land v.(5)) lxor (lnot e land 0xffffffff land v.(6)) in
      let t1 = mask (v.(7) + s1 + ch + k.(t) + w.(t)) in
      let s0 = rotr a 2 lxor rotr a 13 lxor rotr a 22 in
      let maj = (a land v.(1)) lxor (a land v.(2)) lxor (v.(1) land v.(2)) in
      let t2 = mask (s0 + maj) in
      v.(7) <- v.(6); v.(6) <- v.(5); v.(5) <- v.(4);
      v.(4) <- mask (v.(3) + t1);
      v.(3) <- v.(2); v.(2) <- v.(1); v.(1) <- v.(0);
      v.(0) <- mask (t1 + t2)
    done;
    Array.iteri (fun i x -> h.(i) <- mask (h.(i) + x)) v
  done;
  String.concat "" (Array.to_list (Array.map (Printf.sprintf "%08x") h))

(* A JSON string as Python's json.dumps writes it. *)
let json_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  let add_code c =
    if c < 0x10000 then Buffer.add_string buf (Printf.sprintf "\\u%04x" c)
    else begin
      let c = c - 0x10000 in
      Buffer.add_string buf
        (Printf.sprintf "\\u%04x\\u%04x" (0xd800 lor (c lsr 10))
           (0xdc00 lor (c land 0x3ff)))
    end
  in
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
      (match c with
       | 0x22 -> Buffer.add_string buf "\\\""
       | 0x5c -> Buffer.add_string buf "\\\\"
       | 0x0a -> Buffer.add_string buf "\\n"
       | 0x0d -> Buffer.add_string buf "\\r"
       | 0x09 -> Buffer.add_string buf "\\t"
       | 0x08 -> Buffer.add_string buf "\\b"
       | 0x0c -> Buffer.add_string buf "\\f"
       | c when c < 0x20 || c > 0x7e -> add_code c
       | c -> Buffer.add_char buf (Char.chr c));
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0;
  Buffer.add_char buf '"';
  Buffer.contents buf

let lines text =
  let lines = String.split_on_char '\n' text in
  (* A final newline does not start another line. *)
  match List.rev lines with "" :: rest -> List.rev rest | _ -> lines

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let is_word c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
  || c = '_'

(* The kind of a top-level declaration line, if it starts one. *)
let kind line =
  List.find_opt
    (fun k ->
      starts_with k line
      && (String.length line = String.length k
          || not (is_word line.[String.length k])))
    [ "let"; "type"; "module"; "val"; "external" ]

let word_at line i =
  let j = ref i in
  while !j < String.length line && is_word line.[!j] do incr j done;
  String.sub line i (!j - i)

(* The declared name: the first word after the keyword, an attribute, rec,
   an opening parenthesis, type parameters or [type]. *)
let name kind line =
  let len = String.length line in
  let i = ref (String.length kind) in
  let skip_spaces () = while !i < len && line.[!i] = ' ' do incr i done in
  if kind = "let" && !i < len && line.[!i] = '[' then
    i := String.index_from line !i ']' + 1;
  skip_spaces ();
  let rest () = String.sub line !i (len - !i) in
  if kind = "let" && starts_with "rec " (rest ()) then begin
    i := !i + 4;
    skip_spaces ()
  end;
  if kind = "let" && !i < len && line.[!i] = '(' then incr i;
  if kind = "type" && !i < len && line.[!i] = '(' then begin
    i := String.index_from line !i ')' + 1;
    skip_spaces ()
  end;
  if kind = "type" && !i < len && line.[!i] = '\'' then begin
    incr i;
    i := !i + String.length (word_at line !i);
    skip_spaces ()
  end;
  if kind = "module" && starts_with "type " (rest ()) then begin
    i := !i + 5;
    skip_spaces ()
  end;
  word_at line !i

let declaration ~indent (kind, name, first, last, code) =
  Printf.sprintf
    "%s{\n%s  \"kind\": %s,\n%s  \"name\": %s,\n%s  \"start_line\": %d,\n\
     %s  \"end_line\": %d,\n%s  \"code\": %s\n%s}"
    indent indent (json_string kind) indent (json_string name) indent first
    indent last indent (json_string code) indent

let entry ~path ~sha ~role ~declarations =
  Printf.sprintf
    "    {\n      \"path\": %s,\n      \"sha256\": %s,\n      \"role\": %s,\n\
    \      \"declarations\": [\n%s\n      ]\n    }"
    (json_string path) (json_string sha) (json_string role)
    (String.concat ",\n"
       (List.map (declaration ~indent:"        ") declarations))

let () =
  let root = Sys.argv.(1) in
  let read path =
    In_channel.with_open_bin (Filename.concat root path) In_channel.input_all
  in
  let source_entry file =
    let path = "verification/library/" ^ file in
    let text = read path in
    let lines = Array.of_list (lines text) in
    let starts =
      List.filter_map
        (fun i -> Option.map (fun k -> (i, k)) (kind lines.(i)))
        (List.init (Array.length lines) Fun.id)
    in
    let rec decls = function
      | [] -> []
      | (start, k) :: rest ->
        let stop =
          ref
            (match rest with
             | (next, _) :: _ -> next
             | [] -> Array.length lines)
        in
        while !stop > start + 1 && String.trim lines.(!stop - 1) = "" do
          decr stop
        done;
        let code =
          String.concat "\n"
            (Array.to_list (Array.sub lines start (!stop - start)))
        in
        (k, name k lines.(start), start + 1, !stop, code) :: decls rest
    in
    entry ~path ~sha:(sha256 text)
      ~role:(if List.mem file public_contracts then "public_contracts"
             else "semantic_definitions")
      ~declarations:(decls starts)
  in
  let primitive_entry (path, selection) =
    let text = read path in
    let lines = Array.of_list (lines text) in
    entry ~path ~sha:(sha256 text) ~role:"trusted_semantic_primitives"
      ~declarations:
        (List.map
           (fun (name, first, last) ->
             ("trusted_primitive", name, first, last,
              String.concat "\n"
                (Array.to_list
                   (Array.sub lines (first - 1) (last - first + 1)))))
           selection)
  in
  Printf.printf "{\n  \"schema_version\": 1,\n  \"files\": [\n%s\n  ],\n\
                \  \"primitive_code\": [\n%s\n  ]\n}\n"
    (String.concat ",\n" (List.map source_entry files))
    (String.concat ",\n" (List.map primitive_entry primitives))
