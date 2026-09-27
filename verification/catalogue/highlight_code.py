"""Small, lossless OCaml/Vox highlighter for static catalogue pages."""
import html
import re

KEYWORDS = set('and as assert begin class constraint do done downto else end exception external false for fun function functor if in include inherit initializer lazy let match method module mutable new nonrec object of open or private rec sig struct then to true try type val virtual when while with'.split())
MODES = set('ghost total immutable immutable_data unique local global read write portable contended many once aliased unyielding'.split())
TOKEN = re.compile(r'''\(\*|"|\[@{0,2}[A-Za-z_][\w']*|'(?:\\.|[^'\\\n])'|\b(?:0[xX][0-9a-fA-F_]+|[0-9][0-9_]*(?:\.[0-9_]+)?[lLnZ]?)\b|[A-Za-z_][\w']*|(?:->|===|:=|@@|&&|\|\||<=|>=|<>|[=+*/<>@|:;-])''')

def highlight(source):
    output = []
    cursor = 0
    while cursor < len(source):
        match = TOKEN.search(source, cursor)
        if not match:
            output.append(html.escape(source[cursor:]))
            break
        output.append(html.escape(source[cursor:match.start()]))
        token = match.group()
        end = match.end()
        kind = None
        if token == '(*':
            depth = 1
            while end < len(source) and depth:
                if source.startswith('(*', end):
                    depth += 1
                    end += 2
                elif source.startswith('*)', end):
                    depth -= 1
                    end += 2
                else:
                    end += 1
            kind = 'comment'
        elif token == '"':
            while end < len(source):
                if source[end] == '\\':
                    end = min(end + 2, len(source))
                elif source[end] == '"':
                    end += 1
                    break
                else:
                    end += 1
            kind = 'string'
        elif token.startswith("'"):
            kind = 'string'
        elif token.startswith('[') or token in MODES:
            kind = 'mode'
        elif token in KEYWORDS:
            kind = 'keyword'
        elif token[0].isdigit():
            kind = 'number'
        elif token[0].isupper():
            kind = 'constructor'
        elif not token[0].isalnum() and token[0] != '_':
            kind = 'operator'
        escaped = html.escape(source[match.start():end])
        output.append(f'<span class="tok-{kind}">{escaped}</span>' if kind else escaped)
        cursor = end
    return ''.join(output)
