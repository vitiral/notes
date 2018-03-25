import os
import sys
import inspect
from pprint import pprint as pp

stack = None

def foo():
    global stack
    stack = inspect.stack()
    1/0

try:
    f = foo()
except:
    exc_info = sys.exc_info()

print("# STACK")
pp(stack)
print("# EXC_INFO")
pp(exc_info[2])
