"""
I believe this solves the problem in O(n) by using https://www.geeksforgeeks.org/next-greater-element/
"""

def calc_longest_increasing_subset(arr):
  ngi = calc_next_greater_index(arr)

  # stores the length of increasing subset from that index
  incr_sub_len = [0] * len(arr)

  for i in reversed(range(len(arr))):
    # equal to the maximum sub length of
    # itself (1) plus the next larger value
    incr_sub_len[i] = 1 + incr_sub_len[ngi[i]]

  return max(incr_sub_len)

def calc_next_greater_index(arr):
  ngi = list(range(len(arr)))  # default is next-greater-index is "itself"
  decr = []  # store decreasing indexes in a stack

  for i in range(len(arr)):
    while decr and arr[decr[-1]] < arr[i]:
      # arr[i] > arr[decr[i]] so is it's next-greater-index
      ngi[ decr.pop() ] = i
    decr.append(i)

  # items that were not set have "themself" as their next greater index

  return ngi

def test_calc_longest_increasing_subset():
  assert 1 == calc_longest_increasing_subset([0])
  assert 2 == calc_longest_increasing_subset([1,2])
  assert 1 == calc_longest_increasing_subset([2,1])
  assert 2 == calc_longest_increasing_subset([1,2,1])
  assert 4 == calc_longest_increasing_subset([1,2,1,3,1,5])
  assert 4 == calc_longest_increasing_subset([1,2,1,3,1,5,1])
  assert 1 == calc_longest_increasing_subset(list(reversed(range(100))))
  assert 100 == calc_longest_increasing_subset(list(range(100)))
  assert 100 == calc_longest_increasing_subset(list(range(100)) + [1,2,3,4])
  assert 100 == calc_longest_increasing_subset([1,2,3,4] + list(range(100)) + [1,2,3,4])
  assert 101 == calc_longest_increasing_subset([1,2,3,4] + list(range(100)) + [1,2,3,4,1000])
