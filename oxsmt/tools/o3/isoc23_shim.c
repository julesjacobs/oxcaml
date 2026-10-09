#include <stdlib.h>
/* el8 glibc (2.28) predates the C23 __isoc23_* symbol variants that the
   nix-built OxCaml runtime references from caml_float_of_hex /
   caml_float32_of_hex.  Alias them to the classic entry points; the only
   behavioural delta is base-0 binary-prefix ("0b") recognition, which the
   hex-float runtime path never uses. */
long __isoc23_strtol(const char *nptr, char **endptr, int base) {
  return strtol(nptr, endptr, base);
}
long long __isoc23_strtoll(const char *nptr, char **endptr, int base) {
  return strtoll(nptr, endptr, base);
}
unsigned long __isoc23_strtoul(const char *nptr, char **endptr, int base) {
  return strtoul(nptr, endptr, base);
}
unsigned long long __isoc23_strtoull(const char *nptr, char **endptr, int base) {
  return strtoull(nptr, endptr, base);
}
