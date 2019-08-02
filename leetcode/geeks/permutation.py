
def swap(buf, a, b):
    buf[a], buf[b] = buf[b], buf[a]


def permute_string(string):
    print("### PERMUTING:", string)
    str_buf = list(string)
    _permute_string(str_buf, left_i=0)


def _permute_string(str_buf, left_i):
    if left_i >= len(str_buf) - 1:
        print_str_buf(str_buf)
        return

    for i in range(left_i, len(str_buf)):
        swap(str_buf, i, left_i)
        _permute_string(str_buf, left_i+1)
        swap(str_buf, i, left_i)

def print_str_buf(str_buf):
    print(''.join(str_buf))


def fact(n):
    if n <= 1:
        return 1
    res = 1
    for i in range(1, n):
        res *= i
    return res


permute_string("A")
permute_string("AB")
permute_string("ABC")
permute_string("ABCD")
permute_string("ABCDE")
