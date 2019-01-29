import re
import os
import time
from typing import *

word_re = re.compile(r'[0-9a-zA-Z]+')


def parse_path(path: str) -> Dict[str, int]:
    words = {}
    duration = time.time()
    parse_select(words, path)
    duration = time.time() - duration
    print("- INFO: python parsed {} in {}ms".format(path, duration * 1000))
    return words


def parse_dir(words: Dict[str, int], path: str):
    for entry in os.listdir(path):
        parse_select(words, os.path.join(path, entry))



def parse_file(words: Dict[str, int], path: str):
    if not path.endswith('.txt'):
        return

    with open(path, 'r') as f:
        text = f.read()

    for word in word_re.findall(text):
        word = word.lower()
        if word not in words:
            words[word] = 0
        words[word] += 1


def parse_select(words: Dict[str, int], path: str):
    if os.path.islink(path):
        raise ValueError("path is a link")
    elif os.path.isdir(path):
        parse_dir(words, path)
    else:
        parse_file(words, path)
