# Usage: run.sh OCAMLC STDLIB_DIR OCAMLRUN LIBRARY_DIR, from the test's
# build directory, where ./prebuilt holds Pref and Ghost_pref.
#
# Each row of the talk's atomics table deletes one restriction from the
# trusted interface of Verified_atomic (or Unique_cell.Slot). The weakened
# copy is made here, from the library's current source, by one textual
# substitution, and the number of lines it changes is checked, so a copy is
# always "today's interface minus exactly this restriction". The WEAKENED
# COPIES ARE NOT THE LIBRARY: they exist only in this test's build
# directory. Each client is compiled against the real interface and against
# the weakened copy; some weakened builds are also run.
OCAMLC="$1"; STDLIB="$2"; OCAMLRUN="$3"; LIBRARY="$4"
here=$(dirname "$0")
ulimit -c 0 2>/dev/null
FLAGS="-nostdlib -I $STDLIB -extension refinement_types -alert -do_not_spawn_domains"

# compile DIR FILE: compile FILE (copied into DIR) against DIR's modules.
compile() {
  cp "$here/$2" "$1/$2"
  ( cd "$1" && "$OCAMLC" $FLAGS -I ../prebuilt -c "$2" 2>&1 )
  echo "exit $?"
}

# link_run DIR FILE MODULES: link FILE with MODULES (in DIR) and run it.
# Perl waits for the program, so no shell reports a crash (in wording that
# differs between systems); the status is printed instead.
link_run() {
  ( cd "$1" && "$OCAMLC" $FLAGS -I ../prebuilt ../prebuilt/pref.cmo \
      ../prebuilt/ghost_pref.cmo $3 "${2%.ml}.cmo" -o "${2%.ml}.byte" 2>&1 ) ||
    { echo "linking ${2%.ml}.byte failed"; return; }
  ( cd "$1" && perl -e 'system(@ARGV);
      exit($? & 127 ? 128 + ($? & 127) : $? >> 8)' \
      "$OCAMLRUN" "./${2%.ml}.byte" 2>&1 )
  status=$?
  case $status in
    139) echo "exit 139 (SIGSEGV)" ;;
    *) echo "exit $status" ;;
  esac
}

# library DIR NAME: copy NAME.mli and NAME.ml from the library into DIR.
library() {
  mkdir -p "$1"
  cp "$LIBRARY/$2.mli" "$LIBRARY/$2.ml" "$1/"
}

# weaken DIR NAME PERL: apply the substitution PERL to DIR/NAME.mli and
# DIR/NAME.ml, and print the changed lines with their counts.
weaken() {
  for ext in mli ml; do
    f="$1/$2.$ext"
    perl -0777 -pe "$3" "$LIBRARY/$2.$ext" > "$f"
    echo "$2.$ext, weakened copy:"
    diff "$LIBRARY/$2.$ext" "$f" | grep '^[<>]' | sed 's/^\([<>]\) */\1 /' |
      sort | uniq -c | sed 's/^ *//'
  done
}

# build DIR NAME: compile the (real or weakened) library module NAME in DIR.
build() {
  ( cd "$1" && "$OCAMLC" $FLAGS -I ../prebuilt -c "$2.mli" "$2.ml" 2>&1 ) ||
    echo "the library module $2 in $1 does not compile"
}

library real verified_atomic
build real verified_atomic
library real_slot unique_cell
build real_slot unique_cell

echo "#### 1. (a : t) @ local contended on the handle"
library handle verified_atomic
weaken handle verified_atomic 's/\(a : t\) \@ local contended ->/(a : t) ->/g'
build handle verified_atomic
echo "## spawn_client.ml, real interface:"
compile real spawn_client.ml
echo "## spawn_client.ml, weakened copy:"
compile handle spawn_client.ml

echo "#### 2. @@ portable on the operations"
library portable verified_atomic
weaken portable verified_atomic \
  's/\@\@ portable = "caml_vox_atomic_(load|cas|exchange|set|fetch_add)/= "caml_vox_atomic_$1/g'
build portable verified_atomic
echo "## spawn_client.ml, weakened copy:"
compile portable spawn_client.ml

echo "#### 3. caller token @ unique"
library caller verified_atomic
weaken caller verified_atomic \
  's/\(caller : I\.payload Ghost_pref\.token\) \@ unique ghost ->/(caller : I.payload Ghost_pref.token) \@ ghost ->/g'
build caller verified_atomic
echo "## stale_read.ml, real interface:"
compile real stale_read.ml
echo "## stale_read.ml, weakened copy:"
compile caller stale_read.ml
link_run caller stale_read.ml verified_atomic.cmo

echo "#### 4. transition results @ unique"
library result verified_atomic
weaken result verified_atomic \
  's/\(Ghost_pref\.own r\.outgoing\)\} \@ unique\)/(Ghost_pref.own r.outgoing)})/g'
build result verified_atomic
echo "## dup.ml, real interface:"
compile real dup.ml
echo "## dup.ml, weakened copy:"
compile result dup.ml

echo "#### 5. transition @ total"
library total verified_atomic
weaken total verified_atomic \
  's/\} \@ unique\)\n(\s*)\@ immutable total ghost ->/} \@ unique)\n$1\@ immutable ghost ->/g'
build total verified_atomic
echo "## diverge.ml, real interface:"
compile real diverge.ml
echo "## diverge.ml, weakened copy:"
compile total diverge.ml
link_run total diverge.ml verified_atomic.cmo

echo "#### 6. Slot.take's ghost premise"
library slot unique_cell
weaken slot unique_cell \
  's/\(token : \{t : bool Ghost_pref\.token \|\n\s*Ghost_pref\.Heap\.at \(Ghost_pref\.own t\) \(location cell\) === Some true\}\)/(token : bool Ghost_pref.token)/g'
build slot unique_cell
echo "## confuse.ml, real interface:"
compile real_slot confuse.ml
echo "## confuse.ml, weakened copy:"
compile slot confuse.ml
link_run slot confuse.ml unique_cell.cmo
