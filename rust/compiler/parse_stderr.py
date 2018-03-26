from __future__ import print_function
import os
import sys
import json

header = '''\
------------------------------------------
stderr:
------------------------------------------\
'''

text = sys.stdin.read()

if "internal compiler error" in text:
    print(text)
    print("!! ICE, ICE, Baby... can't touch that !!")
    sys.exit(1)

_, text = text.split(header)
text = text.split('\n')

current_file = ''
current_line = -1
current_nodes = []

for l in text:
    try:
        data = json.loads(l)
    except Exception:
        print("~~ ", l)
        continue
    try:
        span = data['spans'][0]
        file = os.path.basename(span['file_name'])
        msg = data['message']
        node = msg[1:].split('(')[0]
        line = span['line_start']

        # print('-', line, msg)
        if file != current_file or line != current_line:
            if line != -1:
                print(current_file, current_line, ','.join(sorted(current_nodes)))
            current_line = line
            current_file = file
            current_nodes = []

        current_nodes.append(node)

    except (KeyError, IndexError) as e:
        print("~~ OtherError:", e, l)

