#!/usr/bin/env bash
# Builds the public Vox site into _build/site/vox:
#
#   index.html    the landing page (verification/site/index.html)
#   catalogue/    the demo catalogue (verification/catalogue/build.py)
#   source/       the source explorer (verification/explorer/build.py)
#   playground/   the in-browser checker (verification/playground/build.sh)
#   talk/         the talk as a steppable deck (verification/talk)
#
#   verification/site/build-site.sh [--revision REV] [--prefix PREFIX]
#                                   [--talk-revision TALK] [--out DIR]
#
# The site describes one commit, REV, which must be on GitHub so that its
# links resolve: by default the newest commit of HEAD on the `vox` branch of
# julesjacobs/oxcaml (fetched first). The catalogue quotes REV with
# `git show`. The playground is built from the checkout, so the checkout may
# differ from REV only in verification/site; the build stops otherwise.
#
# PREFIX is a compiler installed from a commit whose compiler-libs parse the
# demos (the catalogue's line counts and the explorer's demo files need it);
# default _install. It also
# needs what verification/playground/build.sh needs (the oxcaml-5.4.0+oxcaml
# opam switch, node and npm).
#
# The talk is taken from TALK, a commit of the talk branch in this
# repository (default: the head of $talk_branch), with `git archive`, so
# uncommitted work on the deck never reaches the site. Only what the deck
# loads is published: the pages, lib/, the style sheets, fonts, data/*.json
# and the scenes in the running order with their tracks and assets. Scene
# galleries, capture scripts, notes, audio and renders stay out.
#
# The build fails on a broken local link anywhere in the site. Deploy with
# verification/site/deploy.sh.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
revision=
prefix=$root/_install
out=$root/_build/site/vox
talk_branch=jujacobs/vox/talk-v6-20260928
talk_revision=$talk_branch
while [[ $# -gt 0 ]]; do
  case $1 in
    --revision) revision=$2; shift 2 ;;
    --prefix) prefix=$2; shift 2 ;;
    --talk-revision) talk_revision=$2; shift 2 ;;
    --out) out=$2; shift 2 ;;
    *) echo "unknown option $1" >&2; exit 2 ;;
  esac
done
cd "$root"

remote=$(git remote -v | awk '/github\.com[:\/]julesjacobs\/oxcaml(\.git)? \(fetch\)/ {print $1; exit}')
[[ -n $remote ]] || { echo "no remote for github.com/julesjacobs/oxcaml" >&2; exit 1; }
git fetch -q "$remote" vox
published=$(git rev-parse FETCH_HEAD)
if [[ -z $revision ]]; then
  revision=$(git merge-base HEAD "$published")
fi
revision=$(git rev-parse --verify "$revision^{commit}")
git merge-base --is-ancestor "$revision" "$published" \
  || { echo "$revision is not on $remote/vox; push it first" >&2; exit 1; }
if ! git diff --quiet "$revision" -- . ':(exclude)verification/site'; then
  echo "the checkout differs from $revision outside verification/site:" >&2
  git diff --stat "$revision" -- . ':(exclude)verification/site' >&2
  exit 1
fi
echo "== site for $revision in $out"
rm -rf "$out"
mkdir -p "$out"

echo "== catalogue"
python3 verification/catalogue/build.py --revision "$revision" --prefix "$prefix" \
  --output "$out/catalogue" --home-url /vox/

echo "== source explorer"
python3 verification/explorer/build.py --revision "$revision" --prefix "$prefix" \
  --output "$out/source" --catalogue "$out/catalogue" --catalogue-url ../catalogue/

echo "== playground"
verification/playground/build.sh --out "$out/playground" --catalogue-url /vox/catalogue/ \
  --home-url /vox/
# build.sh records HEAD, which may be a commit of this directory's own that
# is not published; the checker's sources are those of $revision (checked above).
python3 - "$out/playground/lib/index.json" "$(git rev-parse --short=10 "$revision")" <<'PY'
import json, sys
path, revision = sys.argv[1:]
index = json.load(open(path))
index['revision'] = revision
json.dump(index, open(path, 'w'))
PY

echo "== talk"
talk_revision=$(git rev-parse --verify "$talk_revision^{commit}")
talk_tmp=$(mktemp -d)
trap 'rm -rf "$talk_tmp"' EXIT
git archive "$talk_revision" verification/talk | tar -x -C "$talk_tmp"
python3 - "$talk_tmp/verification/talk" "$out/talk" <<'PY'
# Copy what the deck loads at run time. Scenes: those of the running order
# (scenes/scenes.json), each with its module, style, speaker track (the
# narration, and its Q&A and Sources, which practice mode shows), the
# helper modules and styles it imports, and assets:
# screen recordings and images at the top of the scene directory, and JSON
# files anywhere in it (layouts and clip lists the scene fetches).
import json, shutil, sys
from pathlib import Path
src, dst = Path(sys.argv[1]), Path(sys.argv[2])
order = json.loads((src / 'scenes/scenes.json').read_text())
keep = ['index.html', 'presenter.html', 'scenes/scenes.json']
keep += [p.name for p in src.glob('*.css')]
keep += [p.relative_to(src).as_posix() for p in (src / 'lib').rglob('*') if p.suffix in ('.js', '.css')]
keep += [p.relative_to(src).as_posix() for p in (src / 'fonts').iterdir() if p.suffix == '.otf' or p.name.startswith('LICENSE')]
keep += [p.relative_to(src).as_posix() for p in (src / 'data').glob('*.json')]
media = {'.mp4', '.webm', '.png', '.jpg', '.svg'}
for entry in order['scenes']:
    scene = src / 'scenes' / entry['id']
    if not (scene / 'scene.js').is_file():
        continue
    for p in scene.rglob('*'):
        top = p.parent == scene
        if p.is_file() and ((top and (p.name == 'track.md' or p.suffix in media)) or p.suffix in ('.js', '.css', '.json')):
            keep.append(p.relative_to(src).as_posix())
for rel in sorted(set(keep)):
    (dst / rel).parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(src / rel, dst / rel)
# No narration audio is published: an empty index, so the deck does not ask
# for one that is not there.
(dst / 'audio').mkdir(exist_ok=True)
(dst / 'audio/index.json').write_text('{"scenes": {}}\n')
print(f'{len(set(keep))} files from', src)
PY
echo "talk deck from $talk_revision"

echo "== landing page"
python3 "$here/landing.py" "$revision" "$out"

echo "== links"
python3 - "$out" <<'PY'
# Every href and src in the site's pages, and every url() in its style
# sheets, must name a file of the site. The site is served at /vox/.
import re, sys
from pathlib import Path, PurePosixPath
from urllib.parse import unquote, urlsplit
site = Path(sys.argv[1])
reference = re.compile(r'''(?:href|src)=["']([^"']*)["']|url\(["']?([^)"']*)["']?\)''')
broken, checked = [], 0
for page in sorted(site.rglob('*')):
    if page.suffix not in ('.html', '.css') or not page.is_file():
        continue
    base = PurePosixPath('/vox') / page.relative_to(site).parent.as_posix()
    for match in reference.finditer(page.read_text(errors='replace')):
        link = match.group(1) if match.group(1) is not None else match.group(2)
        parts = urlsplit(link)
        if parts.scheme or parts.netloc or link.startswith(('#', 'data:')) or not parts.path:
            continue
        path = PurePosixPath(unquote(parts.path) if parts.path.startswith('/') else
                             str(base / unquote(parts.path)))
        resolved = []
        for part in path.parts[1:]:
            if part == '..':
                resolved and resolved.pop()
            elif part != '.':
                resolved.append(part)
        if resolved[:1] != ['vox']:
            broken.append(f'{page.relative_to(site)}: {link} leaves /vox/')
            continue
        target = site.joinpath(*resolved[1:])
        if target.is_dir():
            target = target / 'index.html'
        checked += 1
        if not target.is_file():
            broken.append(f'{page.relative_to(site)}: {link}')
# The talk deck is ES modules: every relative import must resolve, every
# scene of the running order must have its module and track, and every
# file a scene names with ctx.asset('...') must be there.
talk = site / 'talk'
if talk.is_dir():
    importing = re.compile(r'''(?:\bfrom\s*|\bimport\s*\(\s*|^import\s+)['"](\.{1,2}/[^'"]+)['"]''', re.M)
    for module in sorted(talk.rglob('*.js')):
        for match in importing.finditer(module.read_text()):
            checked += 1
            if not (module.parent / match.group(1)).resolve().is_file():
                broken.append(f'{module.relative_to(site)}: import {match.group(1)}')
    import json
    for entry in json.loads((talk / 'scenes/scenes.json').read_text())['scenes']:
        scene = talk / 'scenes' / entry['id']
        for name in ('scene.js', 'track.md'):
            checked += 1
            if not (scene / name).is_file():
                broken.append(f'talk/scenes/{entry["id"]}/{name} is missing')
        if (scene / 'scene.js').is_file():
            for match in re.finditer(r'''asset\(\s*['"]([^'"]+)['"]''', (scene / 'scene.js').read_text()):
                checked += 1
                if not (scene / match.group(1)).is_file():
                    broken.append(f'talk/scenes/{entry["id"]}/scene.js: asset {match.group(1)}')
if broken:
    sys.exit('broken links:\n  ' + '\n  '.join(broken[:50]))
print(f'{checked} local links, none broken')
PY
du -sh "$out"/* | sed "s|$out/||"
echo "built $out from $revision"
