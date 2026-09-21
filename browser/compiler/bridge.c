#include <emscripten.h>
#include "caml/mlvalues.h"
EM_JS(int, vox_solve_js, (const char *text), {
  return Module.voxSolve(UTF8ToString(Number(text)));
});
EM_JS(void, vox_event_js, (const char *text), {
  Module.voxEvent(JSON.parse(UTF8ToString(Number(text))));
});
CAMLprim value caml_vox_browser_solve(value text) {
  return Val_int(vox_solve_js(String_val(text)));
}
CAMLprim value caml_vox_browser_event(value text) {
  vox_event_js(String_val(text));
  return Val_unit;
}
