#!/usr/bin/env bash
# Builds the public Vox site into _build/site/vox:
#
#   index.html    the landing page (verification/site/index.html)
#   catalogue/    the demo catalogue (verification/catalogue/build.py)
#   playground/   the in-browser checker (verification/playground/build.sh)
#
#   verification/site/build-site.sh [--revision REV] [--prefix PREFIX]
#                                   [--out DIR]
#
# The site describes one commit, REV, which must be on GitHub so that its
# links resolve: by default the newest commit of HEAD on the `vox` branch of
# julesjacobs/oxcaml (fetched first). The catalogue quotes REV with
# `git show`. The playground is built from the checkout, so the checkout may
# differ from REV only in verification/site; the build stops otherwise.
#
# PREFIX is a compiler installed from a commit whose compiler-libs parse the
# demos (the catalogue's line counts need it); default _install. It also
# needs what verification/playground/build.sh needs (the oxcaml-5.4.0+oxcaml
# opam switch, node and npm).
#
# The build fails on a broken local link anywhere in the site. Deploy with
# verification/site/deploy.sh.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
revision=
prefix=$root/_install
out=$root/_build/site/vox
while [[ $# -gt 0 ]]; do
  case $1 in
    --revision) revision=$2; shift 2 ;;
    --prefix) prefix=$2; shift 2 ;;
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
if broken:
    sys.exit('broken links:\n  ' + '\n  '.join(broken[:50]))
print(f'{checked} local links, none broken')
PY
du -sh "$out"/* | sed "s|$out/||"
echo "built $out from $revision"
