(* TEST
   readonly_files = "vslice_model.mli vslice_model.ml vslice.mli \
                   vslice_runtime_impl.ml \
                   make_negative_size.ml \
                   get_negative_index.ml get_high_index.ml \
                   slice_get_negative_index.ml slice_get_high_index.ml \
                   slice_set_negative_index.ml slice_set_high_index.ml \
                   split3_reversed_bounds.ml split3_high_bound.ml \
                   final_runtime_use.ml prophecy_runtime_use.ml \
                   final_frame_runtime_use.ml \
                   projection_alias_runtime_use.ml \
                   projection_module_alias_runtime_use.ml \
                   future_projection_runtime_repro.ml \
                   future_projection_uninterpreted.ml";
 flags = "-vox-backend z3";
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
   module = "make_negative_size.ml";
   compiler_output = "make_negative_size.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             make_negative_size.output";
   script;
   script = "grep -Fq 'Vslice.make ~n:(-1) ~value:0' \
             make_negative_size.output";
   script;
   module = "vslice_model.ml";
   ocamlc_byte_exit_status = "0";
   ocamlc.byte;
 }{
   module = "get_negative_index.ml";
   compiler_output = "get_negative_index.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             get_negative_index.output";
   script;
   script = "grep -Fq 'Vslice.get ~array ~index:(-1)' \
             get_negative_index.output";
   script;
 }{
   module = "get_high_index.ml";
   compiler_output = "get_high_index.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             get_high_index.output";
   script;
   script = "grep -Fq 'Vslice.get ~array ~index:2' get_high_index.output";
   script;
 }{
   module = "slice_get_negative_index.ml";
   compiler_output = "slice_get_negative_index.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             slice_get_negative_index.output";
   script;
   script = "grep -Fq 'Vslice.slice_get ~loan ~index:(-1)' \
             slice_get_negative_index.output";
   script;
 }{
   module = "slice_get_high_index.ml";
   compiler_output = "slice_get_high_index.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             slice_get_high_index.output";
   script;
   script = "grep -Fq 'Vslice.slice_get ~loan ~index:2' \
             slice_get_high_index.output";
   script;
 }{
   module = "slice_set_negative_index.ml";
   compiler_output = "slice_set_negative_index.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             slice_set_negative_index.output";
   script;
   script = "grep -Fq 'Vslice.slice_set ~loan ~index:(-1)' \
             slice_set_negative_index.output";
   script;
 }{
   module = "slice_set_high_index.ml";
   compiler_output = "slice_set_high_index.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             slice_set_high_index.output";
   script;
   script = "grep -Fq 'Vslice.slice_set ~loan ~index:2' \
             slice_set_high_index.output";
   script;
 }{
   module = "split3_reversed_bounds.ml";
   compiler_output = "split3_reversed_bounds.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             split3_reversed_bounds.output";
   script;
   script = "grep -Fq '~loan ~first:1 ~last:0' \
             split3_reversed_bounds.output";
   script;
 }{
   module = "split3_high_bound.ml";
   compiler_output = "split3_high_bound.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (disproved)' \
             split3_high_bound.output";
   script;
   script = "grep -Fq '~loan ~first:0 ~last:3' \
             split3_high_bound.output";
   script;
 }{
   module = "final_runtime_use.ml";
   compiler_output = "final_runtime_use.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'The specification-only value Vslice.final cannot be \
             used in executable code.' final_runtime_use.output";
   script;
 }{
   module = "prophecy_runtime_use.ml";
   compiler_output = "prophecy_runtime_use.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'The specification-only value \
             Vslice.prophecy_value cannot be used in executable code.' \
             prophecy_runtime_use.output";
   script;
 }{
   module = "final_frame_runtime_use.ml";
   compiler_output = "final_frame_runtime_use.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'The specification-only value \
             Vslice.final_frame_values cannot be used in executable code.' \
             final_frame_runtime_use.output";
   script;
 }{
   module = "projection_alias_runtime_use.ml";
   compiler_output = "projection_alias_runtime_use.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'The specification-only value Vslice.final cannot be \
             used in executable code.' projection_alias_runtime_use.output";
   script;
 }{
   module = "projection_module_alias_runtime_use.ml";
   compiler_output = "projection_module_alias_runtime_use.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'The specification-only value \
             Projection.prophecy_value cannot be used in executable code.' \
             projection_module_alias_runtime_use.output";
   script;
 }{
   module = "future_projection_runtime_repro.ml";
   compiler_output = "future_projection_runtime_repro.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'The specification-only value \
             Vslice.final_frame_values cannot be used in executable code.' \
             future_projection_runtime_repro.output";
   script;
 }{
   module = "future_projection_uninterpreted.ml";
   compiler_output = "future_projection_uninterpreted.output";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   script = "grep -Fq 'Error: Refinement verification failed (not-proved)' \
             future_projection_uninterpreted.output";
   script;
   script = "grep -Fq 'Vslice.prophecy_value prophecy = []' \
             future_projection_uninterpreted.output";
   script;
 }
*)
