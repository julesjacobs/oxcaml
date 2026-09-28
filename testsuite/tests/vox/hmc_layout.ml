module D = Hm_declarative
module B = Wasm_u32
module C = Wasm_code

type layout = {table_base : B.u32; frame_base : B.u32; stack_base : B.u32;
  heap_base : B.u32; heap_limit : B.u32; max_pc : B.u32;
  stack_capacity : D.index; host_capacity : C.count}
