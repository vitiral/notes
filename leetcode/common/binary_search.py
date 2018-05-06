"""Implement binary search.

Binary search is a simple log(n) search funciton. It operates on a sorted list
and works by:
- looking at the middle value. If it is equal, it returns its index.
- If the middle is larger than the target, it looks at the lower half. The upper half
  is excluded since it is bigger than the middle.
- If the middle is smaller than the target, it looks at the upper half.
"""

def binary_search(nums, target):
    start, end = 0, len(nums)

    while True:
        if end - start <= 3:
            for i in range(start, end):
                if nums[i] == target:
                    return i
            else:
                raise IndexError("{} not found".format(target))

        # middle = (end - start) // 2 + start
        middle = average(start, end)

        if nums[middle] == target:
            return middle
        elif nums[middle] >= target:
            end = middle
        else:
            start = middle


def binary_search(nums, target):
    start, end = 0, len(nums) - 1
    while start <= end:
        mid = (start + end) // 2
        if nums[mid] == target:
            return mid
        elif nums[mid] > target:
            end = mid - 1
        else:
            start = mid + 1
    return -1














def sqrt(x):
    """Compute the sqrt of x as an integer.

    Guess number from 0 - 100 (71)
    - 50: 51 - 100
    - 75: 51 - 74
    - 60: 61 - 74
    """

    low, hi = 0, x

    while low < hi:
        mid = (low + hi) // 2

        result = mid * mid
        if result == x:
            return mid
        elif result > x:
            hi = mid
        else:
            low = mid + 1

    return low - 1




























def guess_number(hi):
    low = 1

    while low <= hi:
        mid = (low + hi) // 2
        result = guess(mid)
        if result == 0:
            return mid
        elif result == 1:
            # the number is higher
            low = mid + 1
        else:
            hi = mid - 1

    raise ValueError("number not found")





















#     [4,5,6,7,0,1,2]
#     [0,1,2,3]
#
#     - find piviot: 4
#     - indexing is challenging:
#         - use "normal" indexes of low=0, hi=len()-1 for algorithm
#         - when getting data, use piviot to "solve" for an index.
#
#     Want:
#     - 0 -> 4
#     - 1 -> 5
#     - 2 -> 6
#     - 3 -> 0
#     - 4 -> 1
#
#     (i + piviot) % 7
#
#     [4, 5, 6, 7, 8, 2, 3]

def search_piviot(nums, target):
    unpivot = UnPiviot(find_piviot(nums), len(nums))
    li, hi = 0, len(nums) - 1

    while li <= hi:
        mid = (li + hi) // 2
        corrected_mid = unpiviot.index(mid)
        value = nums[corrected_mid]
        if value == target:
            return corrected_mid
        elif value > target:
            hi = mid - 1
        else:
            li = mid + 1

    return -1


def find_piviot(nums):
    if len(nums) == 1:
        return 0

    li, hi = 0, len(nums) - 1

    while li <= hi:
        mid = (li + hi) // 2
        if is_piviot(mid):
            return mid
        elif nums[hi] < nums[mid]:
            # piviot happens high
            li = mid + 1
        else:
            hi = mid - 1

    assert False, "unreachable"

def test_find_piviot():
    assert find_piviot([4,5,6,7,0,1,2]) == 4
    # [4,5,6,7,0,1,2]
    # li=0, hi=6
    # - mid = 3
    # - (2 < 7) => True:
    #   - li = 4
    #
    # li=4, hi=6
    # - mid=5
    # - 2 < 1 => False
    #   - hi=4
    #
    # li=4, hi=4

    assert find_piviot([1,2,3]) == 0
    # [1,2,3]
    # li=0, hi=2
    # - mid=1, hi=0

    # li=0, hi=0
    # return 0

    assert find_piviot([2,3,1]) == 2
    # [2,3,1]
    # li=0, hi=2
    # - mid=1, (1<3) => li=2, hi=2
    # return 2



def is_piviot(nums, i):
    """
    * ([0, 1, 2], 0)
    => nums[-1] => 2
    => nums[0] => 0
    => 2 > 0 => True
    """
    return nums[i-1] > nums[i]


def test_is_piviot():
    assert is_piviot([0, 1, 2], 0)
    assert is_piviot([2, 0, 1], 1)
    assert is_piviot([2, 1, 0], 2)


class UnPiviot(object):
    def __init__(self, piviot, length):
        self.piviot = piviot
        self.length = length

    def index(self, i):
        return (i + self.piviot) % self.length





# 8
# - mid=4, result=16, low=0, hi=4
# - mid=2, result=4, low=3, hi=4
# - mid=3, result=9, low=3, hi=3
# result: 2



def average(a, b):
    return (a + b) // 2

