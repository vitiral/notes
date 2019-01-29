import argparse
import difflib
import subprocess
import sys
import time
import os

from typing import *
from ssfi_py import parse
from ssfi_py import make_random


def main():
    parser = argparse.ArgumentParser(description='Process some integers.')
    parser.add_argument('path', nargs='?', help='the path to parse.')
    parser.add_argument('-t', action='store_true', help='ignored for the python program')
    parser.add_argument('-f', action='store_true', help='print full words')
    parser.add_argument(
        '--random', action='store_true',
        help='create a directory with random data at the location.'
    )
    parser.add_argument(
        '--test', action='store_true',
        help='run end to end tests. If path is given run only against that path.'
    )
    args = parser.parse_args()

    path = args.path
    full = args.f

    if args.random:
        make_random.make_random(path)
    elif args.test:
        run_tests(path)
    else:
        print('\n'.join(get_raw_output(path, full)))


def get_raw_output(path, full=False) -> List[str]:
    """Get the output of the program as a list of strings."""
    out = []
    words = parse.parse_path(path).items()
    sort_by = lambda it: it[1]
    if full:
        for word, count in sorted(words):
            out.append("{}\t{}".format(word, count))

    else:
        words = sorted(words, key=sort_by)[-10:]
        for i in range(len(words)-1, -1, -1):
            try:
                word, count = words[i]
            except IndexError:
                break
            out.append("{}\t{}".format(word, count))

    return out


def parse_output(output: str) -> Dict[str, int]:
    """Given raw output, return the count dictionary."""
    out = {}
    for line in output.split('\n'):
        if not line:
            continue
        word, count = line.split('\t')
        out[word] = count
    return out


def run_tests(path):
    print("\n\n##### Running python tests:\n")
    if path is not None:
        run_test(path)
    else:
        run_test('data/simple')
        run_test('data/multi')
        run_test('data/recursive')
        run_test('data/random1')

        random_shallow = 'data/random-shallow'
        if not os.path.exists(random_shallow):
            print("## NOTE: Generating random data at", random_shallow)
            os.mkdir(random_shallow)
            make_random.make_random(random_shallow)

        run_test(random_shallow, threads=1)
        run_test('data/random-shallow', threads=2)
        run_test('data/random-shallow', threads=4)
        run_test('data/random-shallow', threads=8)


def run_test(path, threads=4):
    full = time.time()
    c_impl = subprocess.check_output('./bin/ssfi {} -f -t {}'.format(path, threads), shell=True)
    full = time.time() - full
    py_impl = get_raw_output(path, full=True)
    py_impl = '\n'.join(py_impl) + '\n'
    compare_outputs(path, c_impl, py_impl)

    partial = time.time()
    c_impl = subprocess.check_output('./bin/ssfi {} -t {}'.format(path, threads), shell=True)
    partial = time.time() - partial

    py_impl = get_raw_output(path, full=False)
    py_impl = '\n'.join(py_impl) + '\n'


    compare_outputs(path, c_impl, py_impl)

    print("OK {: <32} threads={} full={:.5}ms partial={:.5}ms".format(
        path, threads,
        full*1000,
        partial*1000,
    ))


def compare_outputs(path, c_impl, py_impl):
    c_impl = c_impl.decode('utf-8')
    c_impl = '\n'.join(sorted(c_impl.split('\n')))
    py_impl = '\n'.join(sorted(py_impl.split('\n')))

    if c_impl != py_impl:
        print("C-IMPL:\n{}\n\n".format(c_impl))
        print("PY-IMPL:\n{}\n\n".format(py_impl))
        print("DIFF:\n")
        sys.stdout.writelines(
            difflib.context_diff(c_impl, py_impl, fromfile='c-impl', tofile='py-impl')
        )
        sys.exit(1)

    c_parsed = parse_output(c_impl)
    py_parsed = parse_output(py_impl)
    if c_parsed != py_parsed:
        raise ValueError("ERR {}: the dictionaries differ".format(path))


if __name__ == '__main__':
    main()
