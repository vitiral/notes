import random

def swap(arr, i, j):
    arr[i], arr[j] = arr[j], arr[i]


def do_test(sorting_fn):
    v = [3, 3, 10, 8, 1, 3]
    expected = sorted(v)
    sorting_fn(v)
    assert v == expected

    for _ in range(10):
        values = [
            random.randint(0, 1000)
            for _ in range(random.randint(0, 30))
        ]
        print("Sorting", sorting_fn, ":", values)
        expected = sorted(values)
        sorting_fn(values)
        assert values == expected


###############################################################################
## INSERTION SORT
"""
Insertion sort is one of the simplest sorting algorithms.

A better name would be "shift sort" -- simply have a sorted array + 1 unsorted
element and continue to shift it left until it is in the right place.
"""

def insertion_sort(arr):
    if not arr:
        return

    for hi in range(1, len(arr)):
        online_insertion_sort(arr, 0, hi)

def online_insertion_sort(arr, li, hi):
    """
    Given a sorted array except the element at `hi`, sorts the array
    using insertion sort.
    """
    if hi - li <= 0:
        return

    # keep track of the smaller value
    while True:
        if hi - 1 < li:
            # no more elements
            break
        if arr[hi - 1] <= arr[hi]:
            # the value to the left is less than value to the right: we're done
            break
        swap(arr, hi - 1, hi)
        hi -= 1

def test_insertion_sort():
    do_test(insertion_sort)


###############################################################################
## QUICKSORT

"""
Implement quicksort from scratch.

The quicksort algorithm is a divide and conquer algorithm
where a piviot point is picked, the piviot is moved to
be guaranteed in the right place and then each half
is sorted via quicksort.

"""

def quicksort(arr):
    _quicksort(arr, 0, len(arr) - 1)

def _quicksort(arr, li, hi):
    if hi <= li:
        return

    pi = resolve_partition(arr, li, hi)
    _quicksort(arr, li, pi - 1)
    _quicksort(arr, pi + 1, hi)



def resolve_partition(arr, li, hi):
    """Ensure that the partition is in the right place
    by ensuring that all indexes < than it are to the left
    and > than it to the right.

    This uses partiation_index = hi for simplicity.

    [7,10, 1, 3, 4]

    """
    piviot = arr[hi]

    # smaller always holds a value that is < piviot
    # (or holds invalid value)
    smaller = li - 1
    for j in range(li, hi):
        if arr[j] <= piviot:
            smaller += 1
            swap(arr, j, smaller)

    # swap the piviot with the value greater than it
    pi = smaller + 1
    swap(arr, pi, hi)
    return pi


def test_quicksort():
    do_test(quicksort)


###############################################################################
## HEAP SORT
"""
Heapsort uses a binary heap to sort more quickly.

The process is:
- max-heapify the array
- continue to:
  - swap the max-val (index=0) with hi, percolate down and hi-=1

Max-heapifying requires that we continuously percolate down the array
"""

def heap_sort(arr):
    hi = len(arr) - 1
    heapify(arr, 0, hi)

    while hi > 0:
        # because of heap, index=0 always has the largest value
        swap(arr, 0, hi)
        hi -= 1
        # ensure heap is kept
        percolate_down(arr, 0, hi)


def test_heap_sort():
    do_test(heap_sort)


def heapify(arr, li, hi):
    if hi - li <= 0:
        return

    for n in range(1, hi+1):
        percolate_up(arr, 0, n)

def max_family(arr, hi, n):
    l = left(arr, hi, n)
    r = right(arr, hi, n)

    max_child = max_node(arr, l, r)
    return max_node(arr, n, max_child)


def percolate_up(arr, li, hi):
    """Percolate the value at hi up.

    This assumes that arr[li:hi] is already a valid max-heap.
    """
    n = hi
    p = parent(arr, li, n)

    while p is not None:
        m = max_node(arr, n, p)
        if m == n:
            swap(arr, n, p)
            n = p
            p = parent(arr, li, n)
        else:
            # the max was the parent, we are done.
            break


def percolate_down(arr, li, hi):
    """Percolate the value at li down.

    This assumes that arr[li+1:hi+1] is already a valid max-heap.
    """
    n = li
    def has_family(n):
        return left(arr, hi, n) is not None

    while has_family(n):
        m = max_family(arr, hi, n)
        if m == n:
            # the max was the node, we are done
            break
        else:
            swap(arr, m, n)
            n = m


def max_node(arr, a, b):
    """Return the larger index, preferring a over b when equal.

    None is treated as always smaller.
    """
    if a is None:
        return b
    elif b is None:
        return a
    elif arr[a] >= arr[b]:
        return a
    else:
        return b


def left(arr, hi, n):
    l = n * 2 + 1
    if l > hi:
        return None
    return l


def right(arr, hi, n):
    r = n * 2 + 2
    if r > hi:
        return None
    return r


def parent(arr, li, n):
    p = (n - 1) // 2
    if p < li:
        return None
    return p
