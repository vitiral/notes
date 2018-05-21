################################################################################
# bit differences

"""
https://www.geeksforgeeks.org/sum-of-bit-differences-among-all-pairs/
Given an integer array of n integers, find sum of bit differences in all pairs
that can be formed from array elements. Bit difference of a pair (x, y) is
count of different bits at same positions in binary representations of x and y.

For example, bit difference for 2 and 7 is 2. Binary representation of 2 is 010
and 7 is 111 ( first and last bits differ in two numbers).

You can find the count of bit differences by XOR-ing the values and then counting
the number of 1's. There are assembly instructions that can count # of 1's but
I don't know them. Alternatively to an assembly instruction you could use
a jump table of 256 values.

Instead I will just count them by
- if xor_value & 1 == 1: count += 1
- xor_value = xor_value >> 1

Which is not performant.

Other than that this looks like a best conceivable runtime of O(n^2) since you
have to operate every value with every other value. I could conceive of being
able to reduce this _slighly_ if you have duplicate elements (and a lot if most
are duplicates.
"""

################################################################################
# https://www.geeksforgeeks.org/modular-exponentiation-power-in-modular-arithmetic/
# Given three numbers x, y and p, compute (x^y) % p.
#
# Examples
#
# Input:  x = 2, y = 3, p = 5
# Output: 3
# Explanation: 2^3 % 5 = 8 % 5 = 3.
#
# Input:  x = 2, y = 5, p = 13
# Output: 6
# Explanation: 2^5 % 13 = 32 % 13 = 6.

"""
Notes: I couldn't even begin to solve this. Below are all notes from the answer:

Here is the modulo relationship I need:
    (a % p) (b % p) ≡  (ab) % p [% p [% p ...]]

In other words, if I am multiplying a * b and moding both I can mod them as
many times as I want.

I'm actually not sure how the heck this applies, since I'm only modding once...
but whatever.
"""

def power(x, e, p):
    """Compute (x ^ e) % p in O(log e) time.

    Obviously this could be done by doing
    - x * x * x ... e times, however this is O(n) time.

        result = x
        for _ in range(e):
            result = result * x
        return result

    However, what if we ramped up x while keeping track with
    y.

    - x         y=9 or 8
    - x^2       y=4
    - x^4       y=2
    - x^8       y=1

    This can be accomplished by doing x=x*x in _each loop_.

    You notice above, y can be _either 9 or 8_.
    Obviously the result is not always x^8. You can
    keep track of the odd values by res = result * x
    when y is odd.

    Using the modulo relationship you can keep modding
    as you go to prevent overflow.
    """
    res = 1
    while e > 0:
        if y % 2 != 0:
            # if y is odd, multiply result with x
            res = (res * x) % p

        e = e // 2
        x = (x * x) % p   # x = x^2
    return res

################################################################################
# https://www.geeksforgeeks.org/meta-strings-check-two-strings-can-become-swap-one-string/
# Meta Strings (Check if two strings can become same after a swap in one string)
#
# Given two strings, the task is to check whether these strings are meta
# strings or not. Meta strings are the strings which can be made equal by exactly
# one swap in any of the strings. Equal string are not considered here as Meta
# strings.
"""
Notes:

Brute force: simply swap every permutation of every character for one string,
checking if they are equal. This is n^2 operation per string since every chr
has to be swapped with every other (n + (n-1) + (n-2) ...)

Best conceivalbe runtime: n -- we have to look at at _least_ every character.

- first: if they are not the same length: no
- second: if they are the same str: no
- third: such strings will be equal when sorted. This reduces a lot of possibilites
  but doesn't guarantee the best conceivable runtime.


- ab   == ba
- aab  == baa
- aac  == aca
- abc  == cba
- aabb == baab

- abc != cab

AGH I need to just solve these problems **manually** first! If I had done
that I could have quickly solved it.

You just compare the strings, counting characters that are different and
recording their indexes. If > 2 return False. Then try swapping only those
characters.
"""

################################################################################
# Find largest word in dictionary by deleting some characters of given string
#
# Giving a dictionary and a string ‘str’, find the longest string in dictionary
# which can be formed by deleting some characters of the given ‘str’.
"""
Notes:

This feels like a backtracking problem.

We can easily walk all possibilities of the string using a trie and then
removing one character at a time.

Input : dict = {"ale", "apple", "monkey", "plea"}
        str = "abpcplea"
Output : apple

abpcplea
- a
- * no `b` found, ignore it
- p
- * no `c`
- p
- ... etc

Try again for b

However, this will be an n^2 runtime, which is the brute force runtime.

No... the brute force runtime is _much_ longer.  Brute force is n!, or trying
every combination of the string.
"""

################################################################################
# Count of strings that can be formed using a, b and c under given constraints
#
# Given a length n, count the number of strings of length n that can be made
# using ‘a’, ‘b’ and ‘c’ with at-most one ‘b’ and two ‘c’s allowed.

"""
EXAMPLE:
Input : n = 3
Output : 19
Below strings follow given constraints:
    aaa aab aac aba abc aca acb acc baa
    bac bca bcc caa cab cac cba cbc cca ccb

Notes:
- This is definitely a combinatorics problem
- For a string length n, the number of permutations of 3 different characters
  would be 3!

Okay... if the problem was given length n, count permutations of at most one b and any a's
it would be trivial: n

- permutations of the b: n
- permutations of one c: n
- permutations of one b and one c: n * n

So the answer would simply be n^3 _except_ we have to care that order doesn't matter
when c is next to c. How many cases is c next to c? => n-1 times.

ccaaa
accaa
aacca
aaacc

Answer: 1 + n + n + n^2 + (n^2 - (n-1)) + (n^3 - (n-1))

1 + 2n + 2n^2 + n^3 - 2n - 2

n^3 + 2n^2 - 1

Wait... this is a programming question! Whoops!

"""



# Generate all binary strings
# Given a string containing 0, 1 and ?(wildcard) generate all strings that can be created by replacing the wildcard characters.
#
# - brute force with recursion O(2^q) q=# of '?':
#   - iterate through string building up regular characters
#   - when '?' is encountered, call the function for both '0' and '1'
#
# The problem with above is we will end up re-computing the latter strings.
#
# - 10?1?
#     - 10010
#     - 10011
#     - 10110
#     - 10111
#
# - 10?1?0?
#
# - XXXXXX0
# - XXXXXX1
#
# - XXXX00Y
# - XXXX01Y
#
# - XX00YYY
# - XX01YYY
#
# - 10YYYYY
#
#
# How can we solve this?
# - we could memoization to cache the result of lower calls. Even better, once the higher
# level call is complete we can discard it's cache.
# - this kind of looks like a tree
#
#                10?1?
#           1001?             1011?
#       10010  10011          10110  10111
#
#
#
#
#
#
#
#
#
#
# def extend_lots_of_strings(chr, array):
#     for i in range(len(array)):
#         # performance smell. Instead always append but reverse at end.
#         array[i] = chr + array[i]
#
# def generate_bin_strs(bin_str):
#     if not bin_str:
#         return []
#     if bin_str == '0' or bin_str == '1':
#         return [bin_str]
#
#     # generate the remaining strings
#     rest = generate_bin_strs(bin_str[i+1:])
#     if bin_str[0] == '?':
#         extend_lots_of_strings('0', rest)
#         extend_lots_of_strings('1', rest)
#
#     else:
#         extend_lots_of_strings(bin_str[0], rest)
#     return rest
#
#
# Notes about this solution:
# - That double call to `extend_lots_of_strings` is what makes this 2^q
# - We could drastically reduce how much time it takes by memoizing. HOWEVER, since we
#   are mutating variables we would have to be careful to copy there or use Cow or other
#   useful data structure (i.e. a rope).
# - Since this is binary data it is tempting to try and use a bit array to solve it in some
#   way. It is possible there are lots of performance boosts there. For instance, we might
#   be able to output masks of some sort and then apply the masks in some clever way.
#
#
#
#
#
# Find the longest substring of k unique characters in a given string
# We want to find the maximum substring that has k unique characters
#
# aabbbcc, k=1 => bbb
# aabbbcc, k=1 => aabbb or bbbcc
#
# The first thing that comes to mind is a sliding window. Move the window from left to right,
# shrinking it from left when it hits a character that is invalid.
#
# Brute force: try every possible permutation of all substrings. Start with beginning, grow until invalid and store every case, then start with next character and grow until invalid, etc. We probably only need to store the maximum known character. I think this is O(n) which is pretty much best conceivable runtime.
#
# Best conceivable runtime: theoretically I feel like you could accomplish this in O(1) or O(log n) with enough upfront work. If we had a sorted array of all values by length it would make this trivial.
#
#
# def count_unique(mystr):
#     # performance improvements:
#     # - use an array of length 26, 'a' at index 0, 'z' at index 25
#     count = {}
#     for c in mystr:
#         if c not in count:
#             count[c] = 0
#         count[c] += 1
#     return sum(count.itervalues())
#
#
# def longest_substring(mystr, k):
#     li, hi, max_i = 0, 1, len(mystr) - 1
#     longest = ''
#     while li <= max_i:
#         cur_str = mystr[li:hi+1]
#         count = count_unique(cur_str)
#         if count > k:
#             li += 1
#         else:
#             if hi - li + 1 > len(longest):
#                 longest = cur_str
#             if hi < max_i:
#                 hi += 1
#     return longest
#
#
#
#
# Maximum absolute difference between the sums of two subarrays
# Given an array of integers, find two non-overlapping contiguous subarrays where the absolute difference between their sum is maximum.
#
# [1, 2, -3]
# => [1, 2], [-3]
#
# I'm a little confused by the scope of the problem. Generating all possible sub arrays is n^2, but generating both is... more.
#
# Even if we _did_ generate all possible subarrays, we would have the problem of needing to select two that don't overlap. So even the brute force solution is quite tricky.
#
#
# Instead, let's solve manually to see if we can come up with any tricks
#
# [1, -1, 3, 5, 10, -6]
#
# When I manually look at this, I start to break up the sub-arrays naturally with my eye. Growing one and shrinking the other dynamically.
#
# - 1+-1 doesn't do anything, discard them
# - 3,5,10 are all positive
# - -6 is a largish negative.
#
# Clearly the answer is [3,5,10], [-6] (you can include the -1, 1 but they don't matter). Clearly I also solved that much faster than n^2 + tricky array lookups.
#
# It seems that a window solution within a window solution is probably our "brute force":
# - start with left-window len=1
# - grow the right-window. The sum can simply grow as well from the previous value.
# - Agh, we have to try _evey_ combination of grow/shrink here. Theoretically the value can go
#   up by shrinking!
# - so now we have a n^4 solution -- both windows have to grow/shrink is much as possible.
#
#
# What if we just did:
# - start with window l=1,r=1. Slide right, trying every value
# - l=1,r=2. Slide right, trying every value
# - l=1,r=3 ... etc
#
#
# It turns out the algorithm is very close to this but I definitely needed some help.
#
# Karne's Algorithm adapted to store the running index:
#
#
#
# def running_max_sum_from_left(arr):
#     sum_arr = [0 for _ in range(len(arr))]
#     current_max = arr[0]
#     max_till_now = arr[0]
#     for i in range(1, len(arr)):
#         current_max = max(arr[i], current_max + arr[i])
#         max_till_now = max(current_max, max_till_now)
#         sum_arr[i] = max_till_now
#     return sum_arr
#
#
# def running_max_sum_from_right(arr):
#     sum_arr = [0 for _ in range(len(arr))]
#     max_i = len(arr) - 1
#     current_max = arr[max_i]
#     max_till_now = arr[max_i]
#     i = max_i - 1
#     while i >= 0:
#         current_max = max(arr[i], current_max + arr[i])
#         max_till_now = max(current_max, max_till_now)
#         sum_arr[i] = max_till_now
#         i -= 1
#     return sum_arr
#
#
# def Negate(object):
#     def __init__(self, arr):
#         self.arr = arr
#
#     def __len__(self):
#         return len(self.arr)
#
#     def __getitem__(self, index):
#         return self.arr[index] * -1
#
# def max_abs_diff(arr):
#     left_positive = running_max_sum_from_left(arr)
#     right_positive = running_max_sum_from_right(arr)
#     left_negative = running_max_sum_from_left(Negate(arr))
#     right_negative = running_max_sum_from_right(Negate(arr))
#
#     max_till_now = -1
#
#     for i in range(0, len(arr) - 1):
#         max_first = abs(left_positive[i] - right_positive[i+1])
#         max_second = abs(left_positive[i] - right_negative[i+1])
#         max_third = abs(left_negative[i] - right_positive[i+1])
#         max_fourth = abs(left_positive[i] - right_negative[i+1])
#         curr_max = max(max_first, max_second, max_third, max_fourth)
#         max_till_now = max(curr_max, max_till_now)
#
#     return max_till_now
#
#
#
# Paper Cut into a minimum number of squares
# Given a paper of size AxB find the minimum number of SQUARES that can be cut into any size.
#
# Input  : 36 x 30
# Output : 5
# Explanation :
# 3 (squares of size 12x12) +
# 2 (squares of size 18x18)
#
#
# 18+18 == 36
# 12 + 16 == 30
# 12 * 3 == 36
#
#
#
# Input  : 4 x 5
# Output : 5
# Explanation :
# 1 (squares of size 4x4) +
# 4 (squares of size 1x1)
#
# Manual attempt for 36 x 30.
# First 30 x 30 leaves 6x30 left, which is 5x (6x6) squares.
# that would be 6 squares, but the minimum is actually 5...
#
#
# This approach: did biggest possible then break up remaining. This approach
# could help us because it gives us our initial minimum. There are lots of
# solutions which create an enormous number of squares.
#
# The next approach would be to look for common divisors(?).
# - 36 and 30 are both divisible by: 2, 3, 4 and 6 the answer is probably squares of only these
#   divisors (or multiples of the divisors). In fact, we probably want to reduce it to only
#   primes.
#
#
# 2 and 3 are the least common primes. Squares will always be... squared.
#
# That is interesting, but let's go back to the first solution:
# - use biggest square first, fine
# - use biggest square that fits two in top
# - keep breaking down, if we end up with more squares than before then stop.
#
# This is going to be a recursive solution.
#
#
#
#
#
#
# def max_squares(width, height):
#     all_primes = compute_all_primes(max(width, height))
#     get_lcds = Lcd(all_primes)
#     return _max_squares(width, height, get_lcds)
#
#
# def _max_squares(width, height, get_lcds: Lcd):
#     """Return a list of squares required to build the given rectangle."""
#     if width == 0 or height == 0:
#         return 0
#     elif width == height:
#         return 1
#
#     if height < width:
#         # make sure height is always greater than width
#         width, height = height, width
#
#     width_lcds = get_lcds.find(width)
#
#     running_max = 0
#     for w_lcd in width_lcd:
#         # find the number of squares for width
#         current_max = width // w_lcd
#
#         current_max += find_squares(width, height - width, get_lcds)
#         if current_max > running_max:
#             running_max = current_max
#
#     return running_max
#
#
# def compute_all_primes(value):
#     """Find the prime numbers using the seive of erathsmus."""
#     max_i = math.sqrt(value)
#     indexes = [1 for _ in range(max_i + 1)]
#     for i in range(2, max_i + 1):
#         for n in range(i+i, max_i+1, i):
#             indexes[n] = 0 # divisible by a prime so not a prime.
#
#     primes = []
#     for i in range(1, max_i + 1):
#         if indexes[i]:
#             primes.append(i)
#
#
# class Lcd(object):
#     def __init__(self, all_primes):
#         self.all_primes
#         self.cache = {}
#
#     def find(self, value)
#         result = self.cache.get(value, [])
#         if result:
#             return result
#         largest_possible_prime = math.sqrt(value)
#         for p in self.all_primes:
#             if p > largest_possible_prime:
#                 result.append(value)
#                 self.cache[value] = result
#                 return result
#
#             if value % prime == 0:
#                 result.append(prime)
#         raise ValueError("value too large")
#
