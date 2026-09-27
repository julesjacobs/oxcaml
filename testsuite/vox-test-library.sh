#!/bin/sh
# Builds the modules that tests list in prebuilt_modules, once and in
# parallel, with the compilers the tests use. ocamltest then links each test
# against exactly the modules it lists instead of compiling them.
#
# usage: vox-test-library.sh OUTPUT OCAMLTEST OCAMLC OCAMLOPT STDLIB PATH...
#
# Each PATH is an absolute test file or a directory of tests. Only the
# modules that those tests list are built. A module's flags are the union of
# -principal and -g over the tests in the same directories that list it, so
# that they do not depend on the selection. Any compiler output fails the
# build, as it failed the tests that compiled the module themselves.

set -eu

[ "$#" -ge 5 ] || {
  echo "usage: $0 OUTPUT OCAMLTEST OCAMLC OCAMLOPT STDLIB PATH..." >&2
  exit 2
}
# Verification needs Z3; without it the tests that use the library skip.
command -v z3 > /dev/null 2>&1 || exit 0

# One build at a time uses an output directory.
if [ -z "${VOX_TEST_LIBRARY_LOCKED-}" ]; then
  mkdir -p "$1"
  VOX_TEST_LIBRARY_LOCKED=1 exec perl -MFcntl=:flock -e '
    open(my $lock, ">", shift) or die "$!\n";
    flock($lock, LOCK_EX) or die "$!\n";
    system(@ARGV);
    exit($? == -1 ? 127 : $? & 127 ? 128 + ($? & 127) : $? >> 8)' \
    "$1/.lock" sh "$0" "$@"
fi

output=$1
ocamltest=$2
ocamlc=$3
ocamlopt=$4
stdlib=$5
shift 5

mkdir -p "$output"
output=$(CDPATH= cd -P "$output" && pwd)
work=$output/.work
rm -rf "$work"
mkdir "$work"

# Remove "dir/.." segments, so that one source has one name.
normalize()
{
  awk -F '\t' 'BEGIN { OFS = "\t" } {
    n = split($3, parts, "/"); m = 0
    for (i = 1; i <= n; i++) {
      if (parts[i] == "..") { if (m > 0) m-- }
      else if (parts[i] != "." && (parts[i] != "" || i == 1))
        out[++m] = parts[i]
    }
    path = out[1]
    for (i = 2; i <= m; i++) path = path "/" out[i]
    $3 = path
    print
  }'
}

# Only tests that mention prebuilt_modules are parsed.
for path in "$@"; do
  if [ -d "$path" ]; then
    find "$path" -type f -name '*.ml' \
      -exec grep -l -e prebuilt_modules -- {} + || true
  else
    grep -l -e prebuilt_modules -- "$path" || true
  fi
done | LC_ALL=C sort -u > "$work/selected"
if [ ! -s "$work/selected" ]; then
  rm -rf "$work"
  exit 0
fi
tr '\n' '\0' < "$work/selected" |
  xargs -0 -n 1 "$ocamltest" -list-sources > "$work/raw-sources"
normalize < "$work/raw-sources" > "$work/selected-sources"
tab=$(printf '\t')
awk -F '\t' '$2 == "prebuilt" { print $1 "\t" $3 }' "$work/selected-sources" |
  while IFS="$tab" read -r test path; do
    if [ -f "$path" ]; then state=found; else state=missing; fi
    printf '%s\t%s\t%s\n' "$state" "$test" "$path"
  done > "$work/prebuilt"
: > "$work/needed.raw"
awk -F '\t' -v needed="$work/needed.raw" '
  {
    base = $3; sub(/.*\//, "", base)
    key = $2 ": cannot find prebuilt module " base
  }
  $1 == "found" { found[key] = 1; print $3 > needed }
  $1 == "missing" { missing[key] = 1 }
  END {
    for (key in missing) if (!(key in found)) { print key; bad = 1 }
    exit bad
  }' "$work/prebuilt" >&2
LC_ALL=C sort -u "$work/needed.raw" > "$work/needed"

sed 's|/[^/]*$||' "$work/selected" | LC_ALL=C sort -u |
  while IFS= read -r directory; do
    grep -l -e prebuilt_modules -- "$directory"/*.ml || true
  done > "$work/all-tests"
tr '\n' '\0' < "$work/all-tests" |
  xargs -0 -n 1 "$ocamltest" -list-sources > "$work/raw-sources"
normalize < "$work/raw-sources" > "$work/all-sources"

# One line per needed source: basename, path, flags.
awk -F '\t' '
  FILENAME == ARGV[1] { needed[$1] = 1; next }
  {
    if ($2 != "prebuilt" || !($3 in needed)) next
    if ($4 ~ /(^| )-principal( |$)/) principal[$3] = 1
    if ($4 ~ /(^| )-g( |$)/) debug[$3] = 1
  }
  END {
    for (path in needed) {
      base = path; sub(/.*\//, "", base)
      flags = (path in debug) ? "-g " : ""
      flags = flags "-extension refinement_types"
      if (path in principal) flags = flags " -principal"
      print base "\t" path "\t" flags
    }
  }' "$work/needed" "$work/all-sources" | LC_ALL=C sort > "$work/modules"

duplicate=$(cut -f1 "$work/modules" | uniq -d | head -n 1)
[ -z "$duplicate" ] || {
  echo "$0: two prebuilt sources are named $duplicate:" >&2
  awk -F '\t' -v name="$duplicate" '$1 == name { print "  " $2 }' \
    "$work/modules" >&2
  exit 2
}

# A unit whose interface is no longer listed loses the stale copy and its
# objects, which would otherwise still constrain it.
awk -F '\t' '{ print $1 }' "$work/modules" > "$work/names"
grep '\.ml$' "$work/names" | while IFS= read -r base; do
  if ! grep -qx "${base}i" "$work/names" && [ -f "$output/${base}i" ]; then
    unit=${base%.ml}
    rm -f "$output/${base}i" "$output/${base}i.flags" \
      "$output/$unit.cmi" "$output/$unit.cmo" "$output/native/${base}i" \
      "$output/native/$unit.cmi" "$output/native/$unit.cmx" \
      "$output/native/$unit.o"
  fi
done

# Copy only changed sources and flags, so that make rebuilds only what they
# affect.
while IFS="$(printf '\t')" read -r base path flags; do
  cmp -s "$path" "$output/$base" || cp "$path" "$output/$base"
  printf '%s\n' "$flags" > "$work/flags"
  cmp -s "$work/flags" "$output/$base.flags" ||
    mv -f "$work/flags" "$output/$base.flags"
done < "$work/modules"

# Every object depends on the compilers, the standard library and this
# script, by content, so that a new compiler rebuilds the library and an
# identical copy does not.
# The default flags of ocamltest.
default_flags='-alert -unsafe_multidomain -alert -do_not_spawn_domains'
default_flags="$default_flags -alert -unsafe_effects -dcanonical-ids"
{
  cksum < "$0"
  cksum < "$ocamlc"
  cksum < "$ocamlopt"
  cat "$stdlib"/*.cmi "$stdlib"/*.cmx | cksum
  printf '%s\n' "$default_flags"
} > "$work/compilers.stamp"
cmp -s "$work/compilers.stamp" "$output/compilers.stamp" ||
  mv -f "$work/compilers.stamp" "$output/compilers.stamp"

cat > "$output/compile.sh" <<'EOF'
#!/bin/sh
# Runs a compilation; any output is an error.
log=$1
shift
if "$@" > "$log" 2>&1 && [ ! -s "$log" ]; then
  rm -f "$log"
  exit 0
fi
cat "$log" >&2
exit 1
EOF

# Each compiler builds and verifies its own tree from the same sources, as the
# tests did, so that neither reads a .cmi written by the other. Neither uses
# -smt-assume-verified, which changes some .cmi files.
mkdir -p "$output/native"
cd "$output"
cut -f1 "$work/modules" > "$work/sources"
while IFS= read -r base; do
  [ -L "native/$base" ] || ln -s "../$base" "native/$base"
done < "$work/sources"
tr '\n' '\0' < "$work/sources" |
  xargs -0 "$ocamlc" -depend -modules > "$work/depend"

# Dependents of a unit with an interface need only its .cmi, except that
# native implementations wait for the .cmx, whose inlining information they
# read.
awk -F '\t' -v ocamlc="$ocamlc" -v ocamlopt="$ocamlopt" \
    -v stdlib="$stdlib" -v default_flags="$default_flags" \
    -v output="$output" '
  FILENAME == ARGV[1] {
    base = $1; flags[base] = $3
    unit = base; sub(/\.mli?$/, "", unit)
    if (base ~ /\.mli$/) mli[unit] = 1; else ml[unit] = 1
    next
  }
  {
    split($0, fields, ":")
    source = fields[1]
    count = split(fields[2], names, " ")
    byte[source] = ""
    native[source] = ""
    for (i = 1; i <= count; i++) {
      unit = tolower(substr(names[i], 1, 1)) substr(names[i], 2)
      if (unit in mli) byte[source] = byte[source] " " unit ".cmi"
      else if (unit in ml) byte[source] = byte[source] " " unit ".cmo"
      if (unit in ml && (source ~ /\.ml$/ || !(unit in mli)))
        native[source] = native[source] " native/" unit ".cmx"
      else if (unit in mli)
        native[source] = native[source] " native/" unit ".cmi"
    }
  }
  function rule(target, source, deps, compiler, directory) {
    printf "%s: %s %s.flags compilers.stamp%s\n", target, source, source, deps
    printf "\t@cd %s && sh %s/compile.sh %s.log %s %s %s -c %s\n",
      directory, output, substr(target, index(target, "/") + 1), compiler,
      common, flags[source], source
  }
  END {
    common = "-nostdlib -I " stdlib " -I . " default_flags
    targets = ""
    for (unit in mli) {
      source = unit ".mli"
      rule(unit ".cmi", source, byte[source], ocamlc, ".")
      rule("native/" unit ".cmi", source, native[source], ocamlopt, "native")
      if (!(unit in ml))
        targets = targets " " unit ".cmi native/" unit ".cmi"
    }
    for (unit in ml) {
      source = unit ".ml"
      interface = (unit in mli) ? " " unit ".cmi" : ""
      rule(unit ".cmo", source, interface byte[source], ocamlc, ".")
      interface = (unit in mli) ? " native/" unit ".cmi" : ""
      rule("native/" unit ".cmx", source, interface native[source], ocamlopt,
        "native")
      targets = targets " " unit ".cmo native/" unit ".cmx"
    }
    print ".DELETE_ON_ERROR:"
    print "all:" targets
  }' "$work/modules" "$work/depend" > "$work/build.mk"
mv -f "$work/build.mk" build.mk

# Units already verified with identical inputs are not verified again.
VOX_VERIFY_CACHE=${VOX_VERIFY_CACHE-$(dirname "$output")/vox-verify-cache}
export VOX_VERIFY_CACHE
jobs=${VOX_BUILD_JOBS:-$(getconf _NPROCESSORS_ONLN)}
rm -rf "$work"
exec make -s -k -j "$jobs" -f build.mk all
