################################################################################
# 9 1
# You are given two sorted arrays, A and B, and A has a large enough buffer at
# the end to hold B.
#
# Write a method to merge B into A in sorted order.
"""
Notes:
The first thing that comes to mind is merge sort. However, that almost
certainly requires extra memory.

Instead I would opt for insertion sort since it is trivially online.
- iterate through B appending 1 value at a time to the last index of A
- call insertion sort on A each time.

This is O(A * B) time complexity.

Let's go back to mergesort since that can be done in O(A + B) complexity.

The only painful thing here is the extra use of memory. Otherwise it is
trivial.

... duh, we need to merge them from the back...
"""

def do_merge(A, B):
    mergesort(A, A, 0, len(A) - len(B) - 1, B, 0, len(B) - 1)


def mergesort(overwrite, left, left_li, left_hi, right, right_li, right_hi):
    left_i, right_i = left_hi, right_hi

    insert_i = len(overwrite) - 1

    while left_i >= left_li and right_i >= right_li:
        if left[left_i] >= right[right_i]:
            overwrite[insert_i] = left[left_i]
            left_i -= 1
        else:
            overwrite[insert_i] = right[right_i]
            right_i -= 1
        insert_i -= 1

    while left_i >= left_li:
        overwrite[insert_i] = left[left_i]
        left_i -= 1
        insert_i -= 1

    while right_i >= right_li:
        overwrite[insert_i] = right[right_i]
        right_i -= 1
        insert_i -= 1

################################################################################
# 9.6
# Given a matrix in which each row and each column is sorted, write a method to
# find an element in it
#
# Assumptions:
# - Rows are sorted left to right in ascending order
# - Columns are sorted top to bottom in ascending order
"""

Notes:
We should be able to do _something_ like binary search in order to
perform elimination.
- Use known maxs and mins to eliminate column/rows. Do this until the scope is reduced.

"""

def row_col_search(matrix, target):
    if not matrix or not matrix[0]:
        return -1
    row = 0
    col = len(matrix[0]) - 1

    while row < len(matrix) and col >= 0:
        if matrix[row][col] == target:
            return True
        elif matrix[row][col] < target:
            # move to larger values
            row += 1
        else:
            # move to smaller values
            col -= 1

    return False
