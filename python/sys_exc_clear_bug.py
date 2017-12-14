# Opened this bug: https://bugs.python.org/issue32317

from __future__ import print_function

import sys

def doclear():
    """This should basically be a noop"""
    sys.exc_clear()

def doclear2():
    try:
        1/0
    except ZeroDivisionError:
        sys.exc_clear()

def test1():
    try:
        1/0
    except ZeroDivisionError:
        exc = sys.exc_info()[0]
        print("first exc:", exc)
        assert exc is ZeroDivisionError

def test2():
    try:
        1/0
    except ZeroDivisionError:
        exc = sys.exc_info()[0]
        print("second exc (before doclear):", exc)
        doclear()
        exc = sys.exc_info()[0]
        print("second exc (after  doclear):", exc)
        assert sys.exc_info()[0] is ZeroDivisionError  # fails

def test3():
    try:
        1/0
    except ZeroDivisionError:
        exc = sys.exc_info()[0]
        print("second exc (before doclear2):", exc)
        doclear2()
        exc = sys.exc_info()[0]
        print("second exc (after  doclear2):", exc)
        assert sys.exc_info()[0] is ZeroDivisionError  # passes

test3()
