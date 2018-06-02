

function hello_py {
python3 - <<EOF
import os
print("hello cwd:", os.getcwd())
EOF
}

function hello_args {
# NOTE: the argv version actually works (yay!)
echo "bash-args: $@"

PY_ARGS="$@" python3 - "$@" <<EOF
import os, sys, shlex
py_args = os.environ['PY_ARGS']
print('py_args={}'.format(repr(py_args)))

args = shlex.split(py_args)
print("args={}".format(repr(args)))
print("argv={}".format(sys.argv))
EOF
}

function py_bash_template {
python3 - "$@" <<EOF
#!/usr/bin/python3
import argparse,os,sys,subprocess as sp
parser = argparse.ArgumentParser(
    prog='template',
    description='example template',
)
parser.add_argument('paths', nargs='+', help='paths')
parser.add_argument('-f', '--flag', action='store_true')
args = parser.parse_args()
print('paths={}, flag={}'.format(args.paths, args.flag))
EOF
}

hello_py
hello_args foo bar -a b -z 'foo bar is zzzz'

printf "\nTEMPLATE no flag\n"
py_bash_template path1 path2 "this path is waaaay too long"

printf "\nTEMPLATE empty flag\n"
py_bash_template --force

printf "\nTEMPLATE help\n"
py_bash_template --help
