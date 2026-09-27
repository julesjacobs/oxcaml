#!/usr/bin/env python3
"""Serves the built playground with the headers it needs.

    python3 verification/playground/serve.py [--port 8769] [DIR]

Z3's WebAssembly build uses threads (SharedArrayBuffer), which browsers allow
only on cross-origin isolated pages, so every response carries
Cross-Origin-Opener-Policy: same-origin and
Cross-Origin-Embedder-Policy: require-corp. DIR defaults to
_build/playground/site.
"""
import argparse
import functools
import http.server
import os

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


class Handler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {**http.server.SimpleHTTPRequestHandler.extensions_map,
                      '.wasm': 'application/wasm', '.js': 'text/javascript',
                      '.ml': 'text/plain; charset=utf-8'}

    def end_headers(self):
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        self.send_header('Cache-Control', 'no-cache')
        super().end_headers()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--port', type=int, default=8769)
    parser.add_argument('directory', nargs='?', default=os.path.join(ROOT, '_build', 'playground', 'site'))
    args = parser.parse_args()
    handler = functools.partial(Handler, directory=args.directory)
    server = http.server.ThreadingHTTPServer(('127.0.0.1', args.port), handler)
    print(f'Serving {args.directory} at http://localhost:{args.port}/')
    server.serve_forever()


if __name__ == '__main__':
    main()
