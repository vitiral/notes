"""Implement quicksort.

Quicksort is a divide and conqure sorting algoithm with
O(n log n) typical performance and n^2 worst case
performance.

It is the the fastest in-place sorting algorithm.

"""

def partition(arr, low, hi):
    """Partition the array at the indexes below low and hi. Hi is the starting
    piviot.

    The partition should ensure that all elements with indexes less than the
    returned piviot are less than the piviot.

    Quote:
    > Target of partitions is, given an array and an element x of array as pivot,
    > put x at its correct position in sorted array and put all smaller elements
    > (smaller than x) before x, and put all greater elements (greater than x)
    > after x. All this should be done in linear time.

    :param list[Comparable] arr: array to be partitioned
    :param int low: lowest index (inclusive) to partition.
    :param int hi: highest index (inclusive) to partition. Note: this is the
        INDEX, not the LENGTH of the array!
    """
    # note: low, hi and piviot are constants
    piviot = arr[hi]

    i = low # place for swapping

    # invariant: at the beginning of the loop i==j, so if arr[i] <= piviot then
    # we KEEP THE VALUE WHERE IT IS by swapping it WITH ITSELF. This continues
    # until we find a place where arr[j == i] <= piviot.
    #
    # This is strange, as normal programer's intuition tells you that things
    # change if we *enter* if-statement.  But really the only way things stay
    # identical is if we NEVER enter the if-statement -- in that case we
    # continually swap elements with *themselves*
    for j in range(low, hi):
        # loop invariant: i == j if no changes have been made
        # loop invariant: ELSE i points to a value > piviot
        if arr[j] <= piviot:
            # loop invariant: i == j if all values have been <= piviot
            # loop invariant: THEREFORE no values get moved/changed
            #                 (we are swapping values with themselves)
            arr[i], arr[j] = arr[j], arr[i]
            i += 1

    # invariant: i == j if no changes have been made
    arr[i], arr[hi] = arr[hi], arr[i]
    return i

def partition_im_dumb(arr, low, hi):
    """BAD BAD BAD -- doesn't work.

    I think the logic in the previous partition is backwards, so I want
    to rewrite it for stupid people like myself.

    :param list[Comparable] arr: array to be partitioned
    :param int low: lowest index (inclusive) to partition.
    :param int hi: highest index (inclusive) to partition. Note: this is the
        INDEX, not the LENGTH of the array!
    """

    # Choose the hi point as the piviot
    p = hi
    piviot = arr[p]

    # Do the loop from hi -> low
    for j in range(high, low - 1):
        if arr[j] > piviot:
            # If the value is greater-than than the piviot,
            # swap it with the piviot
            arr[j], arr[p] = arr[p], arr[j]

            # we have moved the piviot, update it's index
            p = j

            # oh wait, this doesn't work at all...
            # start: [7,1,3,2]
            #    * : [7,1,3,2]
            #    * : [7,1,2,3]
            #    * : [7,1,2,3]
            #    * : [2,1,7,3]  <-- end, that is BAD
            #
            # We can't just "swap it with the piviot", since that could
            # cause the piviot to go below a value that wasn't swapped.
            #
            # Instead we need to keep a moving index of what to swap where
            # ... which is what the other solutiond does.

    # return the index of the piviot
    return p

def quicksort(arr):
    """Use the quicksort algorithm to sort the array.

    # Basics
    Quicksort is a divide-and-conquer algorithm.
    It chooses a *piviot* and then *partitions*
    two halves of the array-range into items less
    than and items greater than the piviot.

    It then reselects piviots on each side.
    """
    _quicksort(arr, 0, len(arr) - 1)

def _quicksort(arr, low, high):
    if low >= high:
        # len=1 or len=0 array is already sorted
        return

    # First partition the array. We know that:
    # - all elements to the left of piviot are <= than it
    # - all elements to the right are > it
    p = partition(arr, low, high)

    # note: value at p never moves again (we already know it is between the
    # correct ranges)

    # recursively sort the lower range
    _quicksort(arr, low=low, high=p - 1)
    # recursively sort the upper range
    _quicksort(arr, low=p + 1, high=high)



# ------
# - TESTS

def check_sort(orig):
    print()
    arr = list(orig)
    quicksort(arr)

    print("orig:{}\nsort:{}".format(orig, arr))
    assert arr == sorted(orig)


def check_partition(orig, expected, piv_i):
    print()
    arr = list(orig)
    res = partition(arr, 0, len(arr) - 1)

    print("orig:{}\npart:{}".format(orig, arr))
    assert arr == expected
    assert res == piv_i

def test_partition():
    # they are ALWAYS < partition, so they are always swapped... with themselves!
    check_partition(orig=[4,2,8,30], expected=[4,2,8,30], piv_i=3)

    #    * : [4,2,8,30,3]  end: i = j = 0, piviot=3
    #    * : [4,2,8,30,3]  end: i = 0, j = 1. arr[j](4) was > piv(3), i doesn't change and no swap
    #    * : [2,4,8,30,3]  end: i = 1, j = 2. arr[j](2) was < piv(3), j swapped with i and i++
    #    * : [2,4,8,30,3]  end: i = 1, j = 3. arr[j](8) was > piv(3), i doesn't change and no swap
    #    * : [2,4,8,30,3]  end: i = 1, j = 4. arr[j](30) was > piv(3), i doesn't change and no swap
    #    * : [2,3,8,30,4]  loop ends, i is ALWAYs swapped with j
    #    return pivit == 1
    check_partition(orig=[4,2,8,30,3], expected=[2,3,8,30,4], piv_i=1)


def test_sort():
    check_sort([4,5,1,2])
    check_sort([8,2,3,5,3,10,2])
    check_sort(list(range(10)))
