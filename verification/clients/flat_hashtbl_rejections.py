import pathlib
import subprocess
import sys

prefix, output = map(pathlib.Path, sys.argv[1:])
cases = {
    'false_reflexivity': ('''module Bad = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = false
  let (reflexive @ total) (x : int) : {u : unit | equal x x} =
    equal_def x x; ()
end
''', 'Refinement could not be proved'),
    'inconsistent_hash': ('''module Bad = struct
  type t = int
  let[@def] (equal @ total) (x : int) (y : int) = true
  let[@def] (hash @ total) (x : int) = x
  let (hash_equal @ total) (x : int) (y : int) :
      {u : unit | not (equal x y) || hash x = hash y} =
    equal_def x y; hash_def x; hash_def y; ()
end
''', 'Refinement could not be proved'),
    'physical_holes': ('let f : int V.Map.t = [None]', 'expected'),
    'hidden_compaction': ('let f = V.Bridge.compact', 'Unbound module'),
    'hidden_invariant': ('module Hidden = V.Spec', 'Unbound module'),
    'hidden_implementation': ('module Hidden = V.Impl', 'Unbound module'),
    'hidden_proof': ('let f = V.Map.same_intro', 'Unbound value'),
    'hidden_representation': ('let f (v : int V.view) = v.model', 'Unbound record field'),
    'stale': ('''let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let changed = V.replace r.table r.view 1 84 r.state in
  V.find_opt r.table r.view 1 (borrow_ changed.#state)
''', 'Refinement could not be proved'),
    'missing_ownership': ('''let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let empty = Ghost_pref.empty () in
  V.find_opt r.table r.view 1 (borrow_ empty)
''', 'Refinement could not be proved'),
    'reused_token': ('''let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let changed = V.replace r.table r.view 1 84 r.state in
  V.replace r.table changed.#view 2 90 r.state
''', 'already been used as unique'),
    'false_lookup': ('''let f () =
  let r : int V.created = V.create (Ghost_pref.empty ()) in
  let changed = V.replace r.table r.view 1 84 r.state in
  let value : {v : int | v = 85} =
    V.find r.table changed.#view 1 (borrow_ changed.#state) in value
''', 'Refinement could not be proved'),
}
for compiler in ('ocamlc', 'ocamlopt'):
    for name, (body, diagnostic) in cases.items():
        source = output / (name + '.ml')
        source.write_text('module V = Flat_hashtbl_public.V\n' + body + '\n')
        result = subprocess.run([str(prefix / 'bin' / compiler), '-nostdlib',
            '-I', str(prefix / 'lib/ocaml'), '-I', str(output / 'public'),
            '-I', str(output), '-extension', 'refinement_types', '-c', str(source)],
            text=True, capture_output=True)
        (output / (compiler + '-' + name + '.log')).write_text(result.stdout + result.stderr)
        assert result.returncode != 0 and diagnostic in result.stderr, (compiler, name, result.stderr)
        print(compiler, name, 'rejected')
