open Ast_mapper

let mapper argv =
  let map_location (location : Location.t) =
    if List.mem "ghost" argv then { location with loc_ghost = true }
    else if List.mem "malformed" argv then
      { location with
        loc_end =
          { location.loc_start with
            pos_cnum = location.loc_start.pos_bol - 1;
          };
      }
    else location
  in
  { default_mapper with location = (fun _ location -> map_location location) }

let () = register "ide_emit_span_mapper" mapper
