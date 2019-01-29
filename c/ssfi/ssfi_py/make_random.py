import os
import string
import random

WORD_CHARS = string.ascii_uppercase + string.ascii_lowercase + string.digits
NON_WORD_CHARS = string.punctuation + string.whitespace

def make_random(path, maxdepth=3, maxtimes=128, num_words=512):
    words = [random_word() for _ in range(num_words)]
    whitespace = [random_non_word() for _ in range(num_words)]
    random_dir(path, maxdepth, maxtimes, words, whitespace)


def random_dir(path, maxdepth, maxtimes, words, whitespace):
    if maxdepth < 0:
        return
    for _ in range(0, random.randint(maxtimes // 2, maxtimes)):
        r = random.random()
        if r < 0.8:
            fpath = os.path.join(path, random_word())
            if r < 0.2:
                random_file(fpath + '.txt', words, whitespace)
            else:
                random_file(fpath, words, whitespace)
        else:
            dname = random_word()
            new_dir = os.path.join(path, dname)
            try:
                os.mkdir(new_dir)
                random_dir(new_dir, maxdepth-1, maxtimes // 2, words, whitespace)
            except OSError:
                pass


def random_stuff(choice, maxlen):
    length = random.randint(1, maxlen)
    return ''.join(random.choice(choice) for _ in range(length))


def random_word(maxlen=36):
    return random_stuff(WORD_CHARS, maxlen)


def random_non_word(maxlen=36):
    return random_stuff(NON_WORD_CHARS, maxlen)


def random_file(path, words, whitespace):
    times = random.randint(0, 1000)
    with open(path, 'w') as f:
        for _ in range(0, times):
            isword = random.random() > 0.5
            if isword:
                f.write(random.choice(words))
            f.write(random.choice(whitespace))
