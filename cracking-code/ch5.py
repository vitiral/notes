"""
# Some pratice problems:

Compute the following by hand:
* 1010 - 0001

 1010
-0001
=====
 1001

* 1010 + 0110

 1010
+0110
=====
0001 0000

* 1100^1010

 1100
^1010
=====
 0110

* 1010 << 1

1 0100

* 1001^1001

 1001
^1001
=====
 0000

* 1001 & 1100
 1001
&1100
=====
 1000

* 1010 >> 1

0101

* 0xFF - 1

 1111 1111 - 1
-        1
==========
 1111 1110
 0xFE

 0xFF
-0x01
=====
 0xFE


* 0xAB + 0x11

 1010 1011
+0001 0001
==========
 1011 1100

 0xAB
+0x11
=====
   BC

0xBC
"""

################################################################################
# PG-55  5.1
# You are given two 32-bit numbers, N and M, and two bit positions, i and j
#
#  Write a method to set all bits between i and j in N equal to M
#  (eg, M becomes a substring of N located at i and starting at j)
#
# EXAMPLE:
#       Input: N = 10000000000, M = 10101, i = 2, j = 6
#       Output: N = 10001010100

"""
Notes:
    The first thing that comes to mind is that we want a mask of some kind.
    We acutally need two:
    - a mask to clear the region (AND with 0's)
    - a mask to set the bits we want (OR with 1's)

AND mask:
    - start with 0xFFFF
    - >> i, this creates 1's
"""

BITS_32 = 0xFFFFFFFF

def set_subbits(M, N, i, j):
    assert j - i + 1 <= 32
    assert j <= 31
    assert i <= j

    clear_mask = create_clear_mask(i, j)
    out = M & clear_mask

    set_mask = create_set_mask(M, i, j)
    return out | set_mask


def create_clear_mask(i, j):
    # need j - i 1's
    ones = get_ones(i, j)

    return BITS_32 & (~( ones << i ))


def create_set_mask(M, i, j):
    ones = get_ones(i, j)
    # clear bits that have too high a length
    m = ones & m
    return m << i


def get_ones(i, j):
    ones = 0x00
    for _ in range(j - i + 1):
        ones = ones << 1
        ones += 1
    return ones


def test_create_clear_mask():
    expected = (BITS_32 ^ 0xFF) | 0x03
    result = create_clear_mask(2, 7)
    assert result == expected


def test_get_ones():
    assert get_ones(0, 0) == 0b0001
    assert get_ones(0, 9) == 0x3FF
    assert get_ones(2, 3) == 0b0011



# After solution was revealed
def get_subbits2(M, N, i, j):
    clear_left = BITS_32 - ((1 << j) - 1)
    clear_right = (1 << i) - 1
    clear_mask = clear_left | clear_right

    set_mask = clear_mask & (N<<i)
    out = M & clear_mask
    return out | set_mask



################################################################################
# PG-59 5.2
# Given a (decimal - eg 3.72) number that is passed in as a string, print
# the binary resentation
#
# If the number can not be represented accurately in binary, print “ERROR”

# um... no. First of all the question was extremly confusing.
# second of all, it's _relatively_ simple... although obviously complex
# in practice. You need to:
# - convert the whole number to binary (simple-ish)
# - convert the decimal to binary-decimal... 0*(1/2)^1 + 1*(1/2)^2 ... etc
#   This can be accomplished by:
#    - multiplying the decimal value by 2
#    - asking if this value is >= 1, if so append 1 else 0
#    - % by 0 (or subtract 1 only if >= 1) and do it again



################################################################################
# 5.3
# Given an integer, print the next smallest and next largest number that have the same
# number of 1 bits in their binary representation
"""
Notes:
- umm... this is kind of a hilarious question first of all. It's practically absurd!

Brute force solution: counting 1's
- get a number
- start with count=0
- if 0x01 & number == 1: count += 1
- number >> 1
- go till number == 0
- this could be sped up with a jump table (1 byte at a time) and I think there are
  assembly instructions that can do it to a 64 or even 128 bit integer.

A brute force with lots of memory/disk:
- have a hash table with a map of #1's => [numbers in order]
- then you do `O(1)` lookup and two `O(log n)` binary searches.

The thing that speaks to me is that:
- the number of 1's change drastically for rollovers, i.e. 0b0111 => 0b1000
- you can almost always _get_ a number with the same number of 1's by bit shifting
  left or right (assumingn the number has padding)
- Except for the case of big rollovers, +1 is a good way to get more 1's


"""

def find_like_1s(num):
    pass


################################################################################
# 5.4 Explain what the following code does:

# ((n & (n-1)) == 0)

"""
Notes:

* n=0x00 = true

 0x00
&0xFF
=====
0x00 => true

* n=0x01 = true
 0x01
&0x00
=====
0x00 => True

* n=0x02 = True

 0x02
&0x01
=====
0x00 => True

* n=0x03 = false
 0x03  0b0011
&0x02  0b0010
=====
0x02 => False


    0x00 true
    0x01 true
    0x02 true
    0x03 false
    0x04 true
    0x05 false
    0x06 false
    0x07 false
    0x08 true

This is true when the primary binary digit rolls over when 1 is subtracted

1 << n - 1

"""


def do_thing(n):
    decremented = n - 1
    result = n & decremented
    return after == 0


################################################################################
# 5.5
# Write a function to determine the number of bits required to convert integer
# A to integer B

# Input: 31, 14         0x1F, 0x0E => 0001_1111, 0000_1110
# Output: 2

"""
Notes:
XOR will result in a number where 1's indicate bits that were different,
then count them.

Or we could have just counted them and subtracted...
"""

################################################################################
# 5.6
# Write a program to swap odd and even bits in an integer with as few
# instructions as possible (eg, bit 0 and bit 1 are swapped, bit 2 and bit 3
# are swapped, etc)

"""
Notes:

Finally a fun question.

Thoughts:
- create an ODD_MASK (i.e. 0b1010_1010_1010_10101) 0xAAAA_AAAA
- create an EVEN_MASK = ODD_MASK >> 1 (or 0x5555_5555)

odds = ODD_MASK & num
evens = EVEN_MASK & num

return (evens << 1) | (odds >> 1)
"""
