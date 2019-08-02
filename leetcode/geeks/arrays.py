import sys
import heapq

def next_int(iter_lines):
    return int(next(iter_lines))

def solve_testcases(solve_testcase):
    iter_lines = iter(sys.stdin.readlines())
    num_testcases = next_int(iter_lines)
    for _ in range(num_testcases):
        print(solve_testcase(iter_lines))


# https://practice.geeksforgeeks.org/problems/missing-number-in-array/
# Missing number in array
# It contains N-1 numbers fron 1...N with one number missing
# The numbers seem to be in sorted order
#
# I can simply add the numbers together, then subtract for what the sum of 1..N is.
# I'm almost positive there is a formula to find that... nothing like just doing it!

def solve_missing_nums():

    def solve_testcase(iter_lines):
        max_num = next_int(iter_lines)
        nums_str = next(iter_lines)
        nums_sum = sum(int(n) for n in nums_str.split())
        full_sum = sum(range(1, max_num+1))

        return full_sum - nums_sum

    return solve_testcases(solve_testcase)

# solve_missing_nums()

# https://practice.geeksforgeeks.org/problems/majority-element/0/?track=md-arrays
# Majority element
# this is simple. Use a dictionary to count the elements. Return the element with a larger count than N/2

def solve_majority_element():
    def solve_testcase(iter_lines):
        length = next_int(iter_lines)
        arr_str = next(iter_lines)

        nums = (int(n) for n in arr_str.split())
        counts = {}
        for num in nums:
            if num not in counts:
                counts[num] = 0
            counts[num] += 1

        for num, count in counts.items():
            if count > length / 2:
                return num
        return -1

    return solve_testcases(solve_testcase)

solve_majority_element()

# Rotate an array with no extra space
# https://practice.geeksforgeeks.org/problems/rotate-a-2d-array-without-using-extra-space/0
#
# Example
# 1 2 3 4 5 6 7 8 9
# becomes
# 7 4 1 8 5 2 9 6 3
#
# 1 2 3
# 4 5 6
# 7 8 9
#
# 7 4 1
# 8 5 2
# 9 6 3
#
# Some notes:
# - 5 (middle) never moves.
# - left corner moved 3 over
#
# Moving to notation arr[i,j]
#
# arr[0,0] -> arr[0,2]
# arr[0,1] -> arr[1,2]
# arr[0,2] -> arr[2,2]
# arr[1,0] -> arr[0,1]
# arr[1,1] -> arr[1,1]
# arr[1,2] -> arr[2,1]
# arr[2,0] -> arr[0,0]
# arr[2,1] -> arr[1,0]
# arr[2,2] -> arr[2,0]

print("wow I suck at arrays")

# Ugly Numbers
# https://practice.geeksforgeeks.org/problems/ugly-numbers/0
#
# I'm supposed to find the 11th ugly number. At it's base it
# seems I can break this problem down into a few problems:
# - Find the prime factors
# - Determine if those prime factors consist of only 2, 3 or 5
#
# This is workable, however it is not optimal. For each number
# I would have to try prime(N/2) to determine it's prime factors.
#
# I am aided by a simple fact -- the ugly primes are not divisible
# by themselves (obviously, they are primes!), so I can simply
# _attempt_ to break any number down by only the ugly primes. If
# it can be thus broken, it is ugly.
#
# Now, for the loop... we can't use the sieve of erathsmus since
# that is for FINDING primes. Instead we want to find non-primes.
# Wait a second... it should be possible to simply count the uggly
# numbers, maybe by running a few iterators simultaniously?
#
# Ah, but any number found this way _might_ be divisible by
# another prime, and we wouldn't know without is_ugly. Therefore
# I think it's easier to just count

def solve_nth_ugly():
    def solve_testcase(iter_lines):
        return return_nth_ugly(next_int(iter_lines))
    return solve_testcases(solve_testcase)

def return_nth_ugly(desired_n):
    # twos = range(2, 2, sys.maxint)
    # threes = range(3, 3, sys.maxint)
    # fives = range(5, 5, sys.maxint)

    count = 0
    for n in range(1, 10**100):
        if is_ugly(n):
            count += 1
        if count == desired_n:
            return n

    assert False


def is_ugly(num):
    num = break_factor(2, num)
    num = break_factor(3, num)
    num = break_factor(5, num)
    return num == 1


def break_factor(factor, num):
    """Break a number down by factor, returning what remains"""
    while num % factor == 0:
        num = num // factor
    return num


