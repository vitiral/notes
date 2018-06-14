"""PG-60: Recursion

Problems that are built off of subproblems.
1. How many subproblems does `f(n)` depend on?
  - binary tree: two, linked list: one, regex: number of possible special characters, etc
2. Solve for a "base case". First compute for `f(0) + f(1)` which are hard coded values.
3. Solve for `f(2)`
4. Understand how to solve for f(3) using f(2) or previous solution.
5. Generalize for `f(n)`
"""


################################################################################
# 8.1: Write a method to generate the nth Fibonacci number
"""
Notes:
fibronachi number is:
 1 1 2 3 ...

f(n) = fn(n-2) + f(n-1)
"""

def fib_bad(n):
    """
    This is technically correct, but uses 2^n memory in stack space (!!).

    It also recomputes numbers needlessly.

                 fib(10)
          (fib(8)  +  fib(9))
    (fib(6)+fib(7))  (fib(7)+fib(8))

    You can already see above that we are doing duplicate work. You can also
    see why it's n^2, each n leads to two branches for _all of n_. This is
    different than a binary tree where the depth is `log2 n`

    """
    if n == 0 or n == 1:
        return 1
    return fib(n - 2) + fib(n - 1)


def fib_mem(mem, n):
    """Compute fib using memoization.
    We first check if the number exists in mem, if it does we use that.

    This reduces our stack size and runtime to to O(n)
    """
    if n == 0 or n == 1:
        return 1
    cached = mem.get(n)
    if cached is not None:
        return cached
    result = fib_mem(mem, n - 2) + fib_mem(mem, n - 1)
    mem[n] = result
    return result


################################################################################
# 8.2:
# Imagine a robot sitting on the upper left hand corner of an NxN grid. The
# robot can only move in two directions: right and down
#
# How many possible paths are there for the robot?

# FOLLOW UP
# Imagine certain squares are “off limits”, such that the robot can not step on
# them.
#
# Design an algorithm to get all possible paths for the robot

"""
Notes:

Let's do some simple cases:

2x2: 2 paths
 ____
|_|_|
|_|_|

1 + 1 + 2


3x3: 6 paths
 _____
|_|_|_|
|_|_|_|
|_|_|_|


(2 + 2) & 4x2

The robot has the following choices available at each square:
- zero paths for the last square
- 0 paths for the right-most and bottom-most squares.
- 2 paths for all other squares

"""

def count_paths(n, squares, row, col):
    """
    This is an n^2 algorithm. Use memoization to reduce.
    """
    if row >= n or col >= n:
        return 0

    return count_paths(squares, row+1, col) + count_paths(squares, row, col+1)


################################################################################
# 8.3: Write a method that returns all subsets of a set.
"""
Notes:
This is actually easier with a list. It should be a generalized form of:

```
subsets = []
for a in A[:]:
    for b in A[1:]
        for c in C[2:]
            subsets.append([a,b,c])
        subsets.append([a, b])
    subsets.append([a])
```
"""

def subsets(existing, prev, input_values, li):
    """Mutates existing to insert the requested subsets."""

    for i in range(li, len(input_values)):
        # continue to "build up" prev for the next run
        local_prev = copy(prev)
        local_prev.insert(input_values[i])
        existing.insert(local_prev)

        subsets(existing, local_prev, input_values, i+1)
