#!/usr/bin/env bash
# Builds the Vox site and publishes it at https://lab.julesjacobs.com/vox/.
#
#   verification/site/deploy.sh [--no-build] [BUILD-SITE OPTIONS]
#
# Builds _build/site/vox with build-site.sh (options such as --prefix are
# passed on; --no-build publishes the existing build), copies it to a
# staging directory on the host `lab` (an ssh alias), then exchanges the
# staging directory with /srv/lab/site/vox in one rename (renameat2
# RENAME_EXCHANGE, `mv --exchange`), so a visitor sees either the old site
# or the new one. The staging directory and the old site live in
# /srv/lab/vox-deploy, outside the served tree but on the same file system
# (the exchange needs that): the old site is kept as
# /srv/lab/vox-deploy/previous. To go back, run on the host
#
#   mv --exchange -T /srv/lab/vox-deploy/previous /srv/lab/site/vox
#
# (running it again goes forward). Caddy serves /srv/lab/site (read-only
# at /srv in its container) and sends the cross-origin isolation headers
# for /vox/playground/*; this script changes nothing else on the host.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
host=lab
site=/srv/lab/site
work=/srv/lab/vox-deploy
url=https://lab.julesjacobs.com/vox
build=1
options=()
for argument in "$@"; do
  case $argument in
    --no-build) build= ;;
    *) options+=("$argument") ;;
  esac
done

if [[ -n $build ]]; then
  "$here/build-site.sh" ${options[@]+"${options[@]}"}
fi
out=$root/_build/site/vox
[[ -f $out/index.html && -f $out/playground/z3-built.wasm ]] \
  || { echo "no site in $out; run without --no-build" >&2; exit 1; }

echo "== upload to $host:$site"
# COPYFILE_DISABLE and --no-xattrs keep macOS metadata (._ files, extended
# attributes) out of the archive. rsync is not installed on the host.
started=$(date +%s)
COPYFILE_DISABLE=1 tar -C "$out" --no-xattrs -cf - . | zstd -q -3 -T0 \
  | ssh "$host" "set -euo pipefail
      exec 9> /tmp/vox-site-deploy.lock
      flock -n 9 || { echo 'another deploy is running' >&2; exit 1; }
      mkdir -p '$work'
      [[ \$(stat -c %d '$work') == \$(stat -c %d '$site') ]] \\
        || { echo '$work and $site are on different file systems' >&2; exit 1; }
      staging=\$(mktemp -d '$work/staging-XXXXXXXX')
      trap 'rm -rf \"\$staging\"' ERR
      zstd -dc | tar -xf - -C \"\$staging\" --no-same-owner --no-same-permissions
      chmod -R u=rwX,go=rX \"\$staging\"
      if [[ -e '$site/vox' ]]; then
        mv --exchange -T \"\$staging\" '$site/vox'
        rm -rf '$work/previous'
        mv -T \"\$staging\" '$work/previous'
      else
        mv -T \"\$staging\" '$site/vox'
      fi
      trap - ERR
      echo \"published \$(du -sh '$site/vox' | cut -f1)\""
echo "uploaded and swapped in $(( $(date +%s) - started )) s"

echo "== check $url"
fail=
expect() {  # URL, then grep patterns that the response headers must match
  local target=$1 headers; shift
  headers=$(curl -sS -o /dev/null -D - -H 'Accept-Encoding: zstd, gzip' "$target")
  for pattern in "$@"; do
    grep -qi "$pattern" <<<"$headers" || { echo "  $target: no '$pattern'" >&2; fail=1; }
  done
}
expect "$url/" '^HTTP/[0-9.]* 200' 'content-type: text/html'
expect "$url/catalogue/" '^HTTP/[0-9.]* 200'
expect "$url/film/" '^HTTP/[0-9.]* 404'
expect "$url/source/" '^HTTP/[0-9.]* 200' 'content-type: text/html'
expect "$url/source/data/tree.json" '^HTTP/[0-9.]* 200' 'content-encoding: \(zstd\|gzip\)'
expect "$url/source/src/typing/typecore.ml.txt" '^HTTP/[0-9.]* 200' 'content-type: text/plain' \
  'content-encoding: \(zstd\|gzip\)'
expect "$url/playground/" '^HTTP/[0-9.]* 200' \
  'cross-origin-opener-policy: same-origin' 'cross-origin-embedder-policy: require-corp'
expect "$url/playground/z3-built.wasm" '^HTTP/[0-9.]* 200' 'content-type: application/wasm' \
  'content-encoding: \(zstd\|gzip\)' 'cross-origin-embedder-policy: require-corp'
expect "$url/playground/probe.txt" '^HTTP/[0-9.]* 404'
expect "${url%/vox}/vox.previous/" '^HTTP/[0-9.]* 404'
[[ -z $fail ]] || { echo "published, but a check failed" >&2; exit 1; }
echo "published $url/"
