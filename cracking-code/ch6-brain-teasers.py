################################################################################
# 6.1
# Add arithmetic operators (plus, minus, times, divide) to make the following
# expression true:
#
# 3 1 3 6 = 8
#
# You can use any parentheses you’d like

"""

 3+1+3-6 = 4
 3+1+3+6 = 16

The result is even:
- even*X == even
- even(*-+)even == even
- odd*odd == odd
- odd-/+odd = even

3 * (1+3) - 6 = 6

3 1 (3 * 6)
3 1 (18)
nope

3 1 (3 - 6)
3 1 (-3)
3*(-1)*(-3) = 9

3 ((1-3) - 6)
3 (-8)

(3+1) / (3/6) = 8
"""

################################################################################
# 6 2
# There is an 8x8 chess board in which two diagonally opposite corners have been cut
# off You are given 31 dominos, and a single domino can cover exactly two
# squares Can you use the 31 dominos to cover the entire board?  Prove your
# answer (by providing an example, or showing why it’s impossible)

"""
First of all 8x8 == 64 squares, remove 2 and that is 62. 31 * 2 == 62 so it is
theoretically possible based on just numbers.

Let's reduce the problem first. Clearly a 2x2 square where you cut the corners
off cannot be covered by a single domino

 ----
 |0 |
 | 0|
 ----

 Let's try a 3x3... well that has an odd number of open squares (9-2=7), so no

 Let's try 4x4
 |0><v|
 |v><^|
 |^ ><|
 | ><0|

It seems clearly impossible.

Basic principle: dominos must be layed in a square or there will be gaps
"""


""" 6.3


  5Q | 3Q
  -------
  5 >|
  2  | 3
     |
     |
     |<3
  3  |<3
  5  | 1
     |
  2 >| 0
  0  | 2
  5  | 2
  4  | 3

"""
