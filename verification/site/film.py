#!/usr/bin/env python3
"""Copies the introduction film into the site with its fonts beside it.

    film.py SOURCE_HTML OUT_DIR CACHE_DIR

The film is one HTML file that loads Inter, JetBrains Mono and Newsreader
from Google Fonts. This writes OUT_DIR/index.html with that stylesheet
replaced by OUT_DIR/fonts.css and the font files in OUT_DIR/fonts, so the
page loads nothing from other origins. The downloads are kept in CACHE_DIR.
"""
import hashlib
import re
import sys
import urllib.request
from pathlib import Path

# Google Fonts serves WOFF2 split by Unicode range to browsers it recognises.
USER_AGENT = ('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 '
              '(KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36')
STYLESHEET = re.compile(r'<link href="(https://fonts\.googleapis\.com/css2\?[^"]*)" rel="stylesheet">')
PRECONNECT = re.compile(r'<link rel="preconnect" href="https://fonts\.(googleapis|gstatic)\.com"[^>]*>\n?')
FONT_URL = re.compile(r'url\((https://fonts\.gstatic\.com/[^)]*)\)')


def fetch(url, cache):
    path = cache / hashlib.sha256(url.encode()).hexdigest()
    if not path.exists():
        request = urllib.request.Request(url, headers={'User-Agent': USER_AGENT})
        with urllib.request.urlopen(request, timeout=60) as response:
            data = response.read()
        path.write_bytes(data)
    return path.read_bytes()


def main():
    source, out, cache = Path(sys.argv[1]), Path(sys.argv[2]), Path(sys.argv[3])
    cache.mkdir(parents=True, exist_ok=True)
    (out / 'fonts').mkdir(parents=True, exist_ok=True)
    page = source.read_text()
    links = STYLESHEET.findall(page)
    assert len(links) == 1, f'expected one Google Fonts stylesheet, found {len(links)}'
    css = fetch(links[0].replace('&amp;', '&'), cache).decode()

    def local(match):
        url = match.group(1)
        name = hashlib.sha256(url.encode()).hexdigest()[:16] + Path(url).suffix
        (out / 'fonts' / name).write_bytes(fetch(url, cache))
        return f'url(fonts/{name})'

    css = FONT_URL.sub(local, css)
    assert 'https://' not in css, 'every font is local'
    (out / 'fonts.css').write_text(css)
    page = PRECONNECT.sub('', page)
    page = STYLESHEET.sub('<link href="fonts.css" rel="stylesheet">', page)
    assert not re.search(r'(src|href)="(https?:)?//', page), 'the film loads nothing from other origins'
    (out / 'index.html').write_text(page)


if __name__ == '__main__':
    main()
