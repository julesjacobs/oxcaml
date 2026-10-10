(* TEST
 flags = "-extension layouts_beta";
 {
   reference = "${test_source_directory}/ghost_fields_runtime.byte.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/ghost_fields_runtime.reference";
   native;
 }
*)

(* Runtime behaviour of ghost record fields: the field occupies no slot,
   construction evaluates real field expressions for their effects only,
   ghost_ expressions in fields are not evaluated, and an all-ghost record
   has kind void: no value exists at run time.

   The slot elision is a native-code guarantee: bytecode represents mixed
   records as ordinary blocks and keeps a placeholder word per ghost field
   (exactly as it does for void-typed fields), hence the separate reference
   for the block sizes below. An all-ghost record has kind void in both
   backends: no value exists at run time. *)

type r = { a : int; p : string @@ ghost; b : int }

let () =
  (* the ghost field occupies no slot: the block has two words *)
  let r = { a = 1; p = "gone"; b = 2 } in
  Printf.printf "size %d\n" (Obj.size (Obj.repr r));
  Printf.printf "a=%d b=%d\n" r.a r.b;
  (* a real field expression is evaluated for its effects, then dropped *)
  let r2 = { a = 3; p = (print_string "field effect\n"; "x"); b = 4 } in
  Printf.printf "a=%d b=%d\n" r2.a r2.b;
  (* an ghost_ field expression is never evaluated *)
  let r3 = { a = 5; p = (ghost_ "y"); b = 6 } in
  Printf.printf "a=%d b=%d\n" r3.a r3.b;
  (* functional update over a ghost field *)
  let r4 = { r3 with a = 7 } in
  Printf.printf "a=%d b=%d\n" r4.a r4.b;
  let r5 = { r with p = (print_string "update effect\n"; "z") } in
  Printf.printf "a=%d b=%d\n" r5.a r5.b;
  (* pattern matching binds placeholders without reading *)
  let { a; p = _; b } = r in
  Printf.printf "a=%d b=%d\n" a b
  ;
  (* the record operand of a ghost-field projection in real code is still
     evaluated; the ghost result is bound without being read *)
  let mk_r () = print_string "proj operand\n"; r in
  let _placeholder = (mk_r ()).p in
  ()

(* the all-ghost wrapper has kind void: no value exists at run time *)
type 'a box = { ghost : 'a @@ ghost }
type holder = { id : int; hidden : string box }

let () =
  let b = { ghost = (print_string "wrap effect\n"; "payload") } in
  let b2 = { ghost = (ghost_ "payload") } in
  (* a void-typed field takes no slot, with no modality on the field *)
  let h = { id = 9; hidden = b } in
  Printf.printf "holder size %d\n" (Obj.size (Obj.repr h));
  Printf.printf "id %d\n" h.id;
  (* void parameters vanish from the calling convention *)
  let use (_x : string box) (n : int) = n + 1 in
  Printf.printf "use %d\n" (use b2 1);
  (* projection at a ghost position; the placeholder is never read *)
  let _hidden = ghost_ b.ghost in
  print_string "done\n"

(* Stdlib.Ghost *)
let () =
  let e : int Ghost.t = { ghost = 42 } in
  (* statement position discards the value, so a ghost projection is fine
     there; [ignore] is not, since its parameter is real *)
  (ghost_ (ignore e.Ghost.ghost));
  print_string "stdlib done\n"

type ('a : any) polymorphic = { hidden : 'a @@ ghost; live : int }

let use_float (_ : float# @ ghost) n = n
let read_float (r : float# polymorphic) =
  let { hidden; live } = r in
  use_float hidden live

let () = assert (read_float { hidden = #1.0; live = 42 } = 42)

module Large_record_update = struct
  type t = {
    real : int;
    g0 : int @@ ghost; g1 : int @@ ghost; g2 : int @@ ghost;
    g3 : int @@ ghost; g4 : int @@ ghost; g5 : int @@ ghost;
    g6 : int @@ ghost; g7 : int @@ ghost; g8 : int @@ ghost;
    g9 : int @@ ghost; g10 : int @@ ghost; g11 : int @@ ghost;
    g12 : int @@ ghost; g13 : int @@ ghost; g14 : int @@ ghost;
    g15 : int @@ ghost; g16 : int @@ ghost; g17 : int @@ ghost;
    g18 : int @@ ghost; g19 : int @@ ghost; g20 : int @@ ghost;
    g21 : int @@ ghost; g22 : int @@ ghost; g23 : int @@ ghost;
    g24 : int @@ ghost; g25 : int @@ ghost; g26 : int @@ ghost;
    g27 : int @@ ghost; g28 : int @@ ghost; g29 : int @@ ghost;
    g30 : int @@ ghost; g31 : int @@ ghost; g32 : int @@ ghost;
    g33 : int @@ ghost; g34 : int @@ ghost; g35 : int @@ ghost;
    g36 : int @@ ghost; g37 : int @@ ghost; g38 : int @@ ghost;
    g39 : int @@ ghost; g40 : int @@ ghost; g41 : int @@ ghost;
    g42 : int @@ ghost; g43 : int @@ ghost; g44 : int @@ ghost;
    g45 : int @@ ghost; g46 : int @@ ghost; g47 : int @@ ghost;
    g48 : int @@ ghost; g49 : int @@ ghost; g50 : int @@ ghost;
    g51 : int @@ ghost; g52 : int @@ ghost; g53 : int @@ ghost;
    g54 : int @@ ghost; g55 : int @@ ghost; g56 : int @@ ghost;
    g57 : int @@ ghost; g58 : int @@ ghost; g59 : int @@ ghost;
    g60 : int @@ ghost; g61 : int @@ ghost; g62 : int @@ ghost;
    g63 : int @@ ghost; g64 : int @@ ghost; g65 : int @@ ghost;
    g66 : int @@ ghost; g67 : int @@ ghost; g68 : int @@ ghost;
    g69 : int @@ ghost; g70 : int @@ ghost; g71 : int @@ ghost;
    g72 : int @@ ghost; g73 : int @@ ghost; g74 : int @@ ghost;
    g75 : int @@ ghost; g76 : int @@ ghost; g77 : int @@ ghost;
    g78 : int @@ ghost; g79 : int @@ ghost; g80 : int @@ ghost;
    g81 : int @@ ghost; g82 : int @@ ghost; g83 : int @@ ghost;
    g84 : int @@ ghost; g85 : int @@ ghost; g86 : int @@ ghost;
    g87 : int @@ ghost; g88 : int @@ ghost; g89 : int @@ ghost;
    g90 : int @@ ghost; g91 : int @@ ghost; g92 : int @@ ghost;
    g93 : int @@ ghost; g94 : int @@ ghost; g95 : int @@ ghost;
    g96 : int @@ ghost; g97 : int @@ ghost; g98 : int @@ ghost;
    g99 : int @@ ghost; g100 : int @@ ghost; g101 : int @@ ghost;
    g102 : int @@ ghost; g103 : int @@ ghost; g104 : int @@ ghost;
    g105 : int @@ ghost; g106 : int @@ ghost; g107 : int @@ ghost;
    g108 : int @@ ghost; g109 : int @@ ghost; g110 : int @@ ghost;
    g111 : int @@ ghost; g112 : int @@ ghost; g113 : int @@ ghost;
    g114 : int @@ ghost; g115 : int @@ ghost; g116 : int @@ ghost;
    g117 : int @@ ghost; g118 : int @@ ghost; g119 : int @@ ghost;
    g120 : int @@ ghost; g121 : int @@ ghost; g122 : int @@ ghost;
    g123 : int @@ ghost; g124 : int @@ ghost; g125 : int @@ ghost;
    g126 : int @@ ghost; g127 : int @@ ghost; g128 : int @@ ghost;
    g129 : int @@ ghost; g130 : int @@ ghost; g131 : int @@ ghost;
    g132 : int @@ ghost; g133 : int @@ ghost; g134 : int @@ ghost;
    g135 : int @@ ghost; g136 : int @@ ghost; g137 : int @@ ghost;
    g138 : int @@ ghost; g139 : int @@ ghost; g140 : int @@ ghost;
    g141 : int @@ ghost; g142 : int @@ ghost; g143 : int @@ ghost;
    g144 : int @@ ghost; g145 : int @@ ghost; g146 : int @@ ghost;
    g147 : int @@ ghost; g148 : int @@ ghost; g149 : int @@ ghost;
    g150 : int @@ ghost; g151 : int @@ ghost; g152 : int @@ ghost;
    g153 : int @@ ghost; g154 : int @@ ghost; g155 : int @@ ghost;
    g156 : int @@ ghost; g157 : int @@ ghost; g158 : int @@ ghost;
    g159 : int @@ ghost; g160 : int @@ ghost; g161 : int @@ ghost;
    g162 : int @@ ghost; g163 : int @@ ghost; g164 : int @@ ghost;
    g165 : int @@ ghost; g166 : int @@ ghost; g167 : int @@ ghost;
    g168 : int @@ ghost; g169 : int @@ ghost; g170 : int @@ ghost;
    g171 : int @@ ghost; g172 : int @@ ghost; g173 : int @@ ghost;
    g174 : int @@ ghost; g175 : int @@ ghost; g176 : int @@ ghost;
    g177 : int @@ ghost; g178 : int @@ ghost; g179 : int @@ ghost;
    g180 : int @@ ghost; g181 : int @@ ghost; g182 : int @@ ghost;
    g183 : int @@ ghost; g184 : int @@ ghost; g185 : int @@ ghost;
    g186 : int @@ ghost; g187 : int @@ ghost; g188 : int @@ ghost;
    g189 : int @@ ghost; g190 : int @@ ghost; g191 : int @@ ghost;
    g192 : int @@ ghost; g193 : int @@ ghost; g194 : int @@ ghost;
    g195 : int @@ ghost; g196 : int @@ ghost; g197 : int @@ ghost;
    g198 : int @@ ghost; g199 : int @@ ghost; g200 : int @@ ghost;
    g201 : int @@ ghost; g202 : int @@ ghost; g203 : int @@ ghost;
    g204 : int @@ ghost; g205 : int @@ ghost; g206 : int @@ ghost;
    g207 : int @@ ghost; g208 : int @@ ghost; g209 : int @@ ghost;
    g210 : int @@ ghost; g211 : int @@ ghost; g212 : int @@ ghost;
    g213 : int @@ ghost; g214 : int @@ ghost; g215 : int @@ ghost;
    g216 : int @@ ghost; g217 : int @@ ghost; g218 : int @@ ghost;
    g219 : int @@ ghost; g220 : int @@ ghost; g221 : int @@ ghost;
    g222 : int @@ ghost; g223 : int @@ ghost; g224 : int @@ ghost;
    g225 : int @@ ghost; g226 : int @@ ghost; g227 : int @@ ghost;
    g228 : int @@ ghost; g229 : int @@ ghost; g230 : int @@ ghost;
    g231 : int @@ ghost; g232 : int @@ ghost; g233 : int @@ ghost;
    g234 : int @@ ghost; g235 : int @@ ghost; g236 : int @@ ghost;
    g237 : int @@ ghost; g238 : int @@ ghost; g239 : int @@ ghost;
    g240 : int @@ ghost; g241 : int @@ ghost; g242 : int @@ ghost;
    g243 : int @@ ghost; g244 : int @@ ghost; g245 : int @@ ghost;
    g246 : int @@ ghost; g247 : int @@ ghost; g248 : int @@ ghost;
    g249 : int @@ ghost; g250 : int @@ ghost; g251 : int @@ ghost;
    g252 : int @@ ghost; g253 : int @@ ghost; g254 : int @@ ghost;
  }

  let x = {
    real = 1;
    g0 = 0; g1 = 0; g2 = 0; g3 = 0; g4 = 0;
    g5 = 0; g6 = 0; g7 = 0; g8 = 0; g9 = 0;
    g10 = 0; g11 = 0; g12 = 0; g13 = 0; g14 = 0;
    g15 = 0; g16 = 0; g17 = 0; g18 = 0; g19 = 0;
    g20 = 0; g21 = 0; g22 = 0; g23 = 0; g24 = 0;
    g25 = 0; g26 = 0; g27 = 0; g28 = 0; g29 = 0;
    g30 = 0; g31 = 0; g32 = 0; g33 = 0; g34 = 0;
    g35 = 0; g36 = 0; g37 = 0; g38 = 0; g39 = 0;
    g40 = 0; g41 = 0; g42 = 0; g43 = 0; g44 = 0;
    g45 = 0; g46 = 0; g47 = 0; g48 = 0; g49 = 0;
    g50 = 0; g51 = 0; g52 = 0; g53 = 0; g54 = 0;
    g55 = 0; g56 = 0; g57 = 0; g58 = 0; g59 = 0;
    g60 = 0; g61 = 0; g62 = 0; g63 = 0; g64 = 0;
    g65 = 0; g66 = 0; g67 = 0; g68 = 0; g69 = 0;
    g70 = 0; g71 = 0; g72 = 0; g73 = 0; g74 = 0;
    g75 = 0; g76 = 0; g77 = 0; g78 = 0; g79 = 0;
    g80 = 0; g81 = 0; g82 = 0; g83 = 0; g84 = 0;
    g85 = 0; g86 = 0; g87 = 0; g88 = 0; g89 = 0;
    g90 = 0; g91 = 0; g92 = 0; g93 = 0; g94 = 0;
    g95 = 0; g96 = 0; g97 = 0; g98 = 0; g99 = 0;
    g100 = 0; g101 = 0; g102 = 0; g103 = 0; g104 = 0;
    g105 = 0; g106 = 0; g107 = 0; g108 = 0; g109 = 0;
    g110 = 0; g111 = 0; g112 = 0; g113 = 0; g114 = 0;
    g115 = 0; g116 = 0; g117 = 0; g118 = 0; g119 = 0;
    g120 = 0; g121 = 0; g122 = 0; g123 = 0; g124 = 0;
    g125 = 0; g126 = 0; g127 = 0; g128 = 0; g129 = 0;
    g130 = 0; g131 = 0; g132 = 0; g133 = 0; g134 = 0;
    g135 = 0; g136 = 0; g137 = 0; g138 = 0; g139 = 0;
    g140 = 0; g141 = 0; g142 = 0; g143 = 0; g144 = 0;
    g145 = 0; g146 = 0; g147 = 0; g148 = 0; g149 = 0;
    g150 = 0; g151 = 0; g152 = 0; g153 = 0; g154 = 0;
    g155 = 0; g156 = 0; g157 = 0; g158 = 0; g159 = 0;
    g160 = 0; g161 = 0; g162 = 0; g163 = 0; g164 = 0;
    g165 = 0; g166 = 0; g167 = 0; g168 = 0; g169 = 0;
    g170 = 0; g171 = 0; g172 = 0; g173 = 0; g174 = 0;
    g175 = 0; g176 = 0; g177 = 0; g178 = 0; g179 = 0;
    g180 = 0; g181 = 0; g182 = 0; g183 = 0; g184 = 0;
    g185 = 0; g186 = 0; g187 = 0; g188 = 0; g189 = 0;
    g190 = 0; g191 = 0; g192 = 0; g193 = 0; g194 = 0;
    g195 = 0; g196 = 0; g197 = 0; g198 = 0; g199 = 0;
    g200 = 0; g201 = 0; g202 = 0; g203 = 0; g204 = 0;
    g205 = 0; g206 = 0; g207 = 0; g208 = 0; g209 = 0;
    g210 = 0; g211 = 0; g212 = 0; g213 = 0; g214 = 0;
    g215 = 0; g216 = 0; g217 = 0; g218 = 0; g219 = 0;
    g220 = 0; g221 = 0; g222 = 0; g223 = 0; g224 = 0;
    g225 = 0; g226 = 0; g227 = 0; g228 = 0; g229 = 0;
    g230 = 0; g231 = 0; g232 = 0; g233 = 0; g234 = 0;
    g235 = 0; g236 = 0; g237 = 0; g238 = 0; g239 = 0;
    g240 = 0; g241 = 0; g242 = 0; g243 = 0; g244 = 0;
    g245 = 0; g246 = 0; g247 = 0; g248 = 0; g249 = 0;
    g250 = 0; g251 = 0; g252 = 0; g253 = 0; g254 = 0;
  }

  let y = { x with real = 2 }
  let z = { y with real = 3 }

  let () = assert ((x.real, y.real, z.real) = (1, 2, 3))
end
