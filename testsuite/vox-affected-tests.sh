#!/bin/sh
# Prints the Vox tests that a change since a base revision can affect, one
# per line, relative to testsuite/tests.
#
# usage: vox-affected-tests.sh ROOT OCAMLTEST OCAMLDEP BASE
#
# A test is affected when it compiles or reads a changed file: its own file,
# a file whose name starts with its own base name (references), a file its
# header names (all_modules, prebuilt_modules, module, modules,
# readonly_files), or a module those files depend on transitively according
# to ocamldep; or when the header of another test that lists one of its
# prebuilt modules changed, since that can change the module's flags.
# Changes include uncommitted ones, and untracked files below
# testsuite/tests and verification/library. When a file changes outside
# testsuite/tests, verification/library, verification/catalogue and
# research, other than a Markdown file, every test is affected.

set -eu

[ "$#" -eq 4 ] || {
  echo "usage: $0 ROOT OCAMLTEST OCAMLDEP BASE" >&2
  exit 2
}
root=$1
ocamltest=$2
ocamldep=$3
base=$4
suites='vox typing-refinement-types'
library=verification/library

work=$(mktemp -d "${TMPDIR:-/tmp}/vox-affected.XXXXXX")
trap 'rm -rf "$work"' 0

git -C "$root" rev-parse --verify --quiet "$base^{commit}" > /dev/null || {
  echo "$0: unknown revision '$base'" >&2
  exit 2
}
# Untracked files count only where tests find sources; elsewhere they are
# build logs and the like, which no build reads until a tracked file names
# them.
{
  git -C "$root" diff --name-only "$base" --
  git -C "$root" ls-files -o --exclude-standard -- testsuite/tests "$library"
} | LC_ALL=C sort -u > "$work/changed"

(cd "$root" &&
  for suite in $suites; do
    "$ocamltest" -find-test-dirs "testsuite/tests/$suite" |
      while IFS= read -r directory; do
        "$ocamltest" -list-tests "$directory" |
          sed "s|^|$directory/|"
      done
  done) > "$work/tests"

if awk '
    /^(testsuite\/tests|research)\// { next }
    /^verification\/(library|catalogue)\// { next }
    /\.md$/ { next }
    { found = 1; exit }
    END { exit !found }' "$work/changed"; then
  sed 's|^testsuite/tests/||' "$work/tests"
  exit 0
fi

# Sources named by the headers, relative to the root.
(cd "$root" && tr '\n' '\0' < "$work/tests" |
  xargs -0 -n 1 "$ocamltest" -list-sources) > "$work/raw-sources"
awk -F '\t' -v root="$root/" '{
    n = split($3, parts, "/"); m = 0
    for (i = 1; i <= n; i++) {
      if (parts[i] == "..") { if (m > 0) m-- }
      else if (parts[i] != "." && (parts[i] != "" || i == 1))
        out[++m] = parts[i]
    }
    path = out[1]
    for (i = 2; i <= m; i++) path = path "/" out[i]
    if (index(path, root) == 1) path = substr(path, length(root) + 1)
    print $1 "\t" path "\t" $2
  }' "$work/raw-sources" > "$work/sources"

# The flags of a prebuilt module depend on every test that lists it, so a
# changed header affects the tests that share its prebuilt modules.
header()
{
  awk '{ print } /\*\)/ { exit }'
}
grep -Fx -f "$work/tests" "$work/changed" | while IFS= read -r test; do
  if git -C "$root" cat-file -e "$base:$test" 2>/dev/null; then
    git -C "$root" show "$base:$test" | header > "$work/old-header"
  else
    : > "$work/old-header"
  fi
  if [ -f "$root/$test" ]; then
    header < "$root/$test" > "$work/new-header"
  else
    : > "$work/new-header"
  fi
  cmp -s "$work/old-header" "$work/new-header" || printf '%s\n' "$test"
done > "$work/headers"

# Module dependencies of every source in the suites and the library.
(cd "$root" &&
  for directory in $library $(printf 'testsuite/tests/%s ' $suites); do
    find "$directory" \( -type f -o -type l \) \
      \( -name '*.ml' -o -name '*.mli' \)
  done) > "$work/candidates"
# A changed library source also changes every symlink to that source.
perl -MCwd=abs_path -e '
  my ($root, $changes_name, $candidates_name) = @ARGV;
  chdir $root or die "$root: $!\n";
  open my $changes, "<", $changes_name or die "$changes_name: $!\n";
  my %changed;
  while (<$changes>) {
    chomp;
    my $path = abs_path($_);
    $changed{$path} = 1 if defined $path;
  }
  open my $files, "<", $candidates_name or die "$candidates_name: $!\n";
  while (<$files>) {
    chomp;
    my $path = abs_path($_);
    print "$_\n" if defined $path && $changed{$path};
  }
' "$root" "$work/changed" "$work/candidates" > "$work/aliases"
cat "$work/aliases" >> "$work/changed"
# Syntax-negative tests still contribute their lexical dependencies.
(cd "$root" && tr '\n' '\0' < "$work/candidates" |
  xargs -0 "$ocamldep" -allow-approx -modules) > "$work/depend" \
  2> "$work/depend-errors" || {
    cat "$work/depend-errors" >&2
    exit 1
  }

awk -F '\t' -v library="$library" '
  FILENAME == ARGV[1] { changed[$0] = 1; next }
  FILENAME == ARGV[2] { exists[$0] = 1; next }
  FILENAME == ARGV[3] {
    split($0, fields, ":")
    deps[fields[1]] = fields[2]
    next
  }
  FILENAME == ARGV[4] { header[$0] = 1; next }
  FILENAME == ARGV[5] {
    seeds[$1] = seeds[$1] "\t" $2
    if ($3 == "prebuilt") {
      prebuilt[$1] = prebuilt[$1] "\t" $2
      if ($1 in header) shared[$2] = 1
    }
    next
  }
  {
    test = $0
    directory = test; sub(/\/[^\/]*$/, "", directory)
    stem = test; sub(/\.ml$/, "", stem)
    delete seen
    count = 0
    stack[++count] = test
    affected = 0
    n = split(prebuilt[test], listed, "\t")
    for (i = 2; i <= n; i++) if (listed[i] in shared) affected = 1
    n = split(seeds[test], listed, "\t")
    for (i = 2; i <= n; i++) stack[++count] = listed[i]
    for (name in changed)
      if (index(name, stem ".") == 1 &&
          substr(name, length(stem) + 2) !~ /\//) affected = 1
    while (count > 0 && !affected) {
      file = stack[count--]
      if (file in seen) continue
      seen[file] = 1
      if (file in changed) { affected = 1; break }
      k = split(deps[file], names, " ")
      for (j = 1; j <= k; j++) {
        unit = tolower(substr(names[j], 1, 1)) substr(names[j], 2)
        found = 0
        for (d = 1; d <= 2 && !found; d++) {
          dir = (d == 1) ? directory : library
          for (e = 1; e <= 2; e++) {
            candidate = dir "/" unit ((e == 1) ? ".mli" : ".ml")
            if (candidate in changed) affected = 1
            if (candidate in exists) {
              found = 1
              if (!(candidate in seen)) stack[++count] = candidate
            }
          }
        }
      }
    }
    if (affected) { sub(/^testsuite\/tests\//, "", test); print test }
  }' "$work/changed" "$work/candidates" "$work/depend" "$work/headers" \
  "$work/sources" "$work/tests"
