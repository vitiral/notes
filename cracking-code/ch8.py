"""PG-60: Recursion


Problems that are built off of subproblems.
1. How many subproblems does `f(n)` depend on?
  - binary tree: two, linked list: one, regex: number of possible special characters, etc
2. Solve for a "base case". First compute for `f(0) + f(1)` which are hard coded values.
3. Solve for `f(2)`
4. Understand how to solve for f(3) using f(2) or previous solution.
5. Generalize for `f(n)`
"""
