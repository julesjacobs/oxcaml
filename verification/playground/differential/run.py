#!/usr/bin/env python3
"""Differential check of the playground against the native Vox compiler.

    python3 verification/playground/differential/run.py \
        --native PREFIX [--site DIR] [--work DIR] [--json FILE] [--jobs N]
        [--browser-bundle] [--only-native] [--compare-with FILE]

Every input is compiled twice, as `ocamlc -extension refinement_types
-color never -c FILE` would compile it alone in a directory: by the native
compiler installed in PREFIX, and by the playground's checker (the built site,
run in Node with check-node.js). The status and the complete compiler output
must agree. For accepted examples, the erased program (-dlambda
-dcanonical-ids) must agree too. The browser may instead decline to check a
file (status 3, for an integer literal beyond 32 bits); such files are listed
as NOT CHECKED, the others that differ as DISAGREE. The exit status is 1 if
any file disagrees.

Inputs:
  - the playground's examples (../examples/*.ml);
  - the boundary cases in cases/*.ml;
  - the tests listed in sample.txt. These are expect tests, which the
    toplevel checks phrase by phrase. Each phrase becomes one file: the
    phrase, preceded by the earlier phrases the test expects to be accepted.

--browser-bundle also writes SITE/differential/browser.html and bundle.json:
open that page (served by serve.py) to repeat the comparison in a browser.

--only-native writes the native results (for another machine's compiler) as
JSON; --compare-with FILE compares the browser with such a file as well.
"""

import argparse
import concurrent.futures
import json
import os
import re
import shutil
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
PLAYGROUND = os.path.dirname(HERE)
ROOT = os.path.dirname(os.path.dirname(PLAYGROUND))
TESTS = os.path.join(ROOT, 'testsuite', 'tests')

EXPECT = re.compile(r'\[%%expect\s*\{\|(.*?)\|\}\s*\]', re.S)
HEADER = re.compile(r'\A\s*\(\* TEST.*?\*\)', re.S)


def phrases(text):
    """The (phrase, expected output) pairs of an expect test."""
    text = HEADER.sub(lambda m: '\n' * m.group(0).count('\n'), text, count=1)
    result, start = [], 0
    for match in EXPECT.finditer(text):
        # The expect block becomes blank lines, keeping line numbers.
        blank = '\n' * match.group(0).count('\n')
        result.append((text[start:match.start()] + blank, match.group(1)))
        start = match.end()
    tail = text[start:]
    if tail.strip():
        result.append((tail, ''))
    return result


def split_test(path, directory):
    """Writes one file per phrase of the expect test at [path]."""
    name = os.path.splitext(os.path.basename(path))[0].replace('-', '_')
    text = open(path).read()
    accepted = ''
    files = []
    for index, (phrase, expected) in enumerate(phrases(text)):
        if not phrase.strip():
            accepted += phrase
            continue
        file = os.path.join(directory, f'{name}_{index + 1:02d}.ml')
        with open(file, 'w') as out:
            out.write(accepted + phrase)
        files.append(file)
        # A phrase the test expects to be rejected is left out of later
        # files; blank lines keep their line numbers.
        accepted += re.sub(r'[^\n]', '', phrase) if 'Error' in expected else phrase
    return files


def native(prefix, file, lambda_=False):
    directory = os.path.join(os.path.dirname(file), '.native-' + os.path.basename(file))
    shutil.rmtree(directory, ignore_errors=True)
    os.makedirs(directory)
    shutil.copy(file, directory)
    env = dict(os.environ, OCAMLLIB=os.path.join(prefix, 'lib', 'ocaml'))
    env.pop('VOX_VERIFY_CACHE', None)
    env.pop('OCAMLPARAM', None)
    command = [os.path.join(prefix, 'bin', 'ocamlc.opt'), '-extension', 'refinement_types',
               '-color', 'never', '-c', os.path.basename(file)]
    if lambda_:
        command[1:1] = ['-dlambda', '-dcanonical-ids']
    started = time.time()
    run = subprocess.run(command, cwd=directory, env=env, capture_output=True, text=True)
    elapsed = time.time() - started
    shutil.rmtree(directory, ignore_errors=True)
    return {'status': run.returncode, 'output': run.stderr + run.stdout, 'seconds': elapsed}


def browser(site, files, lambda_=False):
    command = ['node', os.path.join(HERE, 'check-node.js'), site, '--json']
    if lambda_:
        command.append('--lambda')
    run = subprocess.run(command + files, capture_output=True, text=True)
    if run.returncode != 0:
        sys.exit('check-node.js failed:\n' + run.stderr)
    results = {}
    for line in run.stdout.splitlines():
        result = json.loads(line)
        results[result['file']] = result
    return results


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--native', required=True, help='prefix of a native Vox install')
    parser.add_argument('--site', default=os.path.join(ROOT, '_build', 'playground', 'site'))
    parser.add_argument('--work', default=os.path.join(ROOT, '_build', 'playground', 'differential'))
    parser.add_argument('--only-native', action='store_true')
    parser.add_argument('--compare-with', help='native results of another machine (JSON)')
    parser.add_argument('--json', help='write all results to this file')
    parser.add_argument('--browser-bundle', action='store_true',
                        help='also write SITE/differential/, to run the comparison in a browser')
    parser.add_argument('--jobs', type=int, default=4, help='parallel native compilations')
    args = parser.parse_args()
    args.native = os.path.abspath(args.native)
    args.site = os.path.abspath(args.site)

    shutil.rmtree(args.work, ignore_errors=True)
    inputs = []
    for group, source in [('examples', os.path.join(PLAYGROUND, 'examples')),
                          ('cases', os.path.join(HERE, 'cases'))]:
        directory = os.path.join(args.work, group)
        os.makedirs(directory)
        for name in sorted(os.listdir(source)):
            if name.endswith('.ml'):
                shutil.copy(os.path.join(source, name), directory)
                inputs.append((group, os.path.join(directory, name)))
    sample = [line.split('#')[0].strip() for line in open(os.path.join(HERE, 'sample.txt'))]
    for test in filter(None, sample):
        directory = os.path.join(args.work, 'tests', os.path.dirname(test))
        os.makedirs(directory, exist_ok=True)
        for file in split_test(os.path.join(TESTS, test), directory):
            inputs.append(('tests', file))

    files = [file for _, file in inputs]
    with concurrent.futures.ThreadPoolExecutor(args.jobs) as pool:
        outputs = pool.map(lambda file: native(args.native, file), files)
    results = {file: {'group': group, 'native': output}
               for (group, file), output in zip(inputs, outputs)}
    if args.only_native:
        relative = {os.path.relpath(f, args.work): r['native'] for f, r in results.items()}
        json.dump(relative, open(args.json, 'w'), indent=1)
        print(f'{len(files)} native results written to {args.json}')
        return

    if args.browser_bundle:
        # browser.html checks these in a real browser, with the worker the
        # page uses, and compares them with the native results.
        directory = os.path.join(args.site, 'differential')
        os.makedirs(directory, exist_ok=True)
        shutil.copy(os.path.join(HERE, 'browser.html'), directory)
        bundle = [{'key': os.path.relpath(file, args.work), 'source': open(file).read(),
                   'status': results[file]['native']['status'],
                   'output': results[file]['native']['output']} for file in files]
        json.dump(bundle, open(os.path.join(directory, 'bundle.json'), 'w'))
        print(f'wrote {directory}/browser.html and bundle.json ({len(bundle)} files)')

    checked = browser(args.site, files)
    for file in files:
        results[file]['browser'] = checked[file]
    examples = [file for group, file in inputs if group == 'examples']
    lambdas = browser(args.site, examples, lambda_=True)
    for file in examples:
        if results[file]['native']['status'] == 0:
            results[file]['native_lambda'] = native(args.native, file, lambda_=True)
            results[file]['browser_lambda'] = lambdas[file]
    other = json.load(open(args.compare_with)) if args.compare_with else {}

    disagreements = 0
    by_group = {}
    for file in files:
        result = results[file]
        n, b = result['native'], result['browser']
        same = n['status'] == b['status'] and n['output'] == b['output']
        if 'native_lambda' in result:
            same_lambda = result['native_lambda']['output'] == (result['browser_lambda']['output']
                                                                 + (result['browser_lambda']['lambda'] or ''))
            result['same_lambda'] = same_lambda
            same = same and same_lambda
        result['same'] = same
        key = os.path.relpath(file, args.work)
        if key in other:
            o = other[key]
            result['same_other'] = o['status'] == b['status'] and o['output'] == b['output']
            result['native_platforms_agree'] = o['status'] == n['status'] and o['output'] == n['output']
        stats = by_group.setdefault(result['group'], {'files': 0, 'agree': 0, 'accepted': 0,
                                                      'rejected': 0, 'limitation': 0})
        stats['files'] += 1
        stats['agree'] += same and b['status'] != 3
        stats[{0: 'accepted', 2: 'rejected', 3: 'limitation'}.get(b['status'], 'rejected')] += 1
        if b['status'] == 3:
            # Not a verdict: the browser build declined to check the file.
            limitation = b['output'].split('Error: ')[-1].splitlines()[0]
            print(f'NOT CHECKED {key} (native status {n["status"]}): {limitation}')
        elif not same:
            disagreements += 1
            print(f'DISAGREE {key}: native status {n["status"]}, browser status {b["status"]}')
            if n['output'] != b['output']:
                print('--- native\n' + n['output'] + '--- browser\n' + b['output'])
            if not result.get('same_lambda', True):
                print('--- erased program differs')
        if key in other and not result['same_other']:
            print(f'DISAGREE with {args.compare_with}: {key}'
                  + ('' if result['native_platforms_agree'] else ' (the two native compilers also disagree)'))
    for group, stats in by_group.items():
        print(f'{group}: {stats["agree"]}/{stats["files"] - stats["limitation"]} checked files agree '
              f'({stats["accepted"]} accepted, {stats["rejected"]} rejected; '
              f'{stats["limitation"]} not checked in the browser)')
    if other:
        agree = sum(1 for r in results.values() if r.get('same_other'))
        compared = sum(1 for r in results.values() if 'same_other' in r)
        print(f'browser vs {os.path.basename(args.compare_with)}: {agree}/{compared} agree')
    print('timings (browser build in Node, ms): '
          + ', '.join(f'{os.path.basename(f)} {results[f]["browser"]["totalMs"]:.0f}' for f in examples))
    if args.json:
        json.dump({os.path.relpath(f, args.work): r for f, r in results.items()},
                  open(args.json, 'w'), indent=1)
    sys.exit(1 if disagreements else 0)


if __name__ == '__main__':
    main()
