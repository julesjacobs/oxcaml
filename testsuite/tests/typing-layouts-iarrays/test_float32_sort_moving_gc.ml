(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 readonly_files = "gen_u_iarray.ml test_gen_u_iarray.ml";
 modules = "${readonly_files}";
 flambda2;
 {
   flags = "-Oclassic";
   native;
 }{
   flags = "-O3";
   native;
 }
*)

open Stdlib_stable
open Stdlib_upstream_compatible

module Float32_u_array0 :
  Gen_u_iarray.S0 with type element_t = float32#
                   and type ('a : any) array_t = 'a iarray
                   and type mutable_t = float32# array = struct
  type element_t = float32#
  type ('a : any) array_t = 'a iarray
  type mutable_t = float32# array

  type element_arg = unit -> element_t
  type t = element_t iarray

  let max_length = Sys.max_array_length

  external length : element_t iarray -> int = "%array_length"
  external get : element_t iarray -> int -> element_t = "%array_safe_get"
  external unsafe_get : element_t iarray -> int -> element_t =
    "%array_unsafe_get"

  let get t i = let a = get t i in fun () -> a
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a

  external makearray_dynamic : int -> element_t -> element_t array =
    "%makearray_dynamic"

  let unsafe_create_mutable i =
    if i < 0 || i > max_length then invalid_arg "unsafe_create_mutable";
    makearray_dynamic i (Float32_u.of_float32 Float32.zero)

  external unsafe_set_mutable : element_t array -> int -> element_t -> unit =
    "%array_unsafe_set"

  let unsafe_set_mutable t i e = unsafe_set_mutable t i (e ())

  external unsafe_get_mutable : element_t array -> int -> element_t =
    "%array_unsafe_get"

  let unsafe_get_mutable t i = let a = unsafe_get_mutable t i in fun () -> a

  let blit_to_mutable src src_off dst dst_off len =
    for i = 0 to len - 1 do
      unsafe_set_mutable dst (dst_off + i) (unsafe_get src (src_off + i))
    done

  external freeze : element_t array -> element_t iarray = "%array_to_iarray"

  let empty () = freeze (unsafe_create_mutable 0)

  let compare_element x y =
    Float32.compare (Float32_u.to_float32 (x ())) (Float32_u.to_float32 (y ()))
end

module Float32_u_array = Gen_u_iarray.Make (Float32_u_array0)

module A = Test_gen_u_iarray.Make_boxed (struct
    module M = Float32_u_array

    module I = struct
      include Float32

      let max_val = max_float
      let min_val = min_float
      let rand x = of_float (Random.float (to_float x))
      let print f = Printf.printf "%f" (to_float f)
    end

    module E = struct
      let to_boxed x = Float32_u.to_float32 (x ())
      let of_boxed x () = Float32_u.of_float32 x
    end
  end)

let check_sort len =
  let a = A.init len (fun i -> Float32.of_int (len - 1 - i)) in
  let b = A.sort Float32.compare a in
  assert (A.length b = len);
  for i = 0 to len - 1 do
    assert (Float32.compare (A.get b i) (Float32.of_int i) = 0)
  done

let () =
  Gc.set { (Gc.get ()) with minor_heap_size = 1024; space_overhead = 1 };
  check_sort 1000;
  check_sort 1001
