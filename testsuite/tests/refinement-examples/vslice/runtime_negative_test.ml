(* TEST
   readonly_files = "vslice_model.mli vslice_model.ml vslice.mli \
                   vslice_runtime_impl.ml \
                   slice_set_negative_index.ml slice_set_high_index.ml \
                   split3_reversed_bounds.ml split3_high_bound.ml \
                   final_runtime_use.ml prophecy_runtime_use.ml \
                   final_frame_runtime_use.ml \
                   projection_alias_runtime_use.ml \
                   projection_module_alias_runtime_use.ml \
                   future_projection_runtime_repro.ml \
                   future_projection_uninterpreted.ml";
 setup-ocamlc.byte-build-env;
 script = "cp vslice_runtime_impl.ml vslice.ml";
 script;
   module = "vslice_model.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "vslice.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "vslice.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 {
   module = "slice_set_negative_index.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   module = "vslice_model.ml";
   ocamlc_byte_exit_status = "0";
   ocamlc.byte;
 }{
   module = "slice_set_high_index.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "split3_reversed_bounds.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "split3_high_bound.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "final_runtime_use.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "prophecy_runtime_use.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "final_frame_runtime_use.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "projection_alias_runtime_use.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "projection_module_alias_runtime_use.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "future_projection_runtime_repro.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }{
   module = "future_projection_uninterpreted.ml";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
 }
*)
