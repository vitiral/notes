################################################################################
# PG-46: write code to reverse a c-style string

from typing import *

def reverse_str(s: List[str]):
    # first find the length
    len = 0
    for c in s:
        if c == '\0':
            break
        len += 1

    if len == 0:
        return s

    # now reverse the string
    li, hi = 0, len - 1
    while li < hi:
        swap(s, li, hi)
        li += 1
        hi -= 1


def swap(arr, a, b):
    arr[a], arr[b] = arr[b], arr[a]


def cstr(s):
    out = list(s)
    out.append('\0')
    return out


def assert_it(v, expected):
    reverse_str(v)
    assert v == expected


def test_it():
    expected = ['d', 'c', 'b', 'a', '\0']
    assert_it(cstr('abcd'), expected)

    expected = ['c', 'b', 'a', '\0']
    assert_it(cstr('abc'), expected)


################################################################################
# PG-46 1.6: Given an image represented by an NxN matrix, where each
# pixel in the image is 4 bytes, write a method to rotate the image by 90 degrees.
# Can you do this in place?
#
# Thoughts:
# 4 bytes == u32
#
# So this is an image represented by N N-length arrays
#
# 3x3:
# [
#   [ 1, 2, 3],
#   [ 4, 5, 6],
#   [ 7, 8, 9],
# ]
#
# [
#   [ 7, 4, 1],
#   [ 8, 5, 2],
#   [ 9, 6, 3],
# ]
#
# Create second array, and copy like this:
#
# f_c = 0
# s_r = 0
# while f_c < len_c:
#   s_c = 0
#   f_r = len_r - 1
#   while f_r >= 0:
#     second[s_r][s_c] = first[f_r][f_c]
#     s_c += 1
#     f_r -= 1
#   f_c += 1
#   s_r += 1
#
#
#
# for f_r in range(0, len()-1):
#   s_c = 0
#   for f_c in range(0, len_c-1):
#     second[s_r][s_c] = first[f_r][f_c]
#     s_c += 1
#   s_r -= 1

