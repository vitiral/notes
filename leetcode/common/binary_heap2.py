"""Binary heap operations


General parameters
:param in n: node index.
:param int li: low index (inclusive)
:param int hi: high index (inclusive)
:param (int, int) r: range of the array from [li,hi] inclusive that
    we are considering "the heap". This affects child calculations in
    every way.

It is really important that *every* operation know:
- What the array range is `r == (li, hi)`
- What node they are refering to (and not confuse this with `li`)
"""


def swap(arr, i, j):
    arr[i], arr[j] = arr[j], arr[i]


def left(r, n):
    li, _ = r
    return 2 * (n - li) + 1 + li


def right(r, n):
    li, _ = r
    return 2 * (n - li) + 2 + li


def parent(r, n):
    """Return the parent or None."""
    li, _ = r
    p = (n - li - 1) // 2 + li
    if p < li:
        return None
    return p


def min_family(arr, r, n):
    """Index of minum value of parent and children.

    If parent is minimum, returns n.
    """
    _, hi = r
    left_i = left(r, n)
    if left_i > hi:
        # node has no children
        return n

    min_i = n
    right_i = right(r, n)

    if arr[left_i] < arr[min_i]:
        min_i = left_i

    if right_i <= hi and arr[right_i] < arr[min_i]:
        min_i = right_i

    return min_i


def max_family(arr, r, n):
    """Index of maximum value of parent and children.

    If parent is maximum, returns n.
    """
    _, hi = r
    left_i = left(r, n)
    if left_i > hi:
        # node has no children
        return n

    max_i = n
    right_i = right(r, n)

    if arr[left_i] > arr[max_i]:
        max_i = left_i

    if right_i <= hi and arr[right_i] > arr[max_i]:
        max_i = right_i

    return max_i


def is_heap(arr, r, n):
    """Return whether the node is a valid max-heap within the given range."""
    li, hi = r
    if hi - li <= 0:
        # trivial case: 1 or 0 len array
        return True

    left_i = left(r, n)
    if left_i > hi:
        # node has no children
        return True

    if arr[left_i] > arr[n]:
        # root node is less than left
        return False

    right_i = right(r, n)
    if right_i <= hi and arr[right_i] > arr[n]:
        # root node is less than right
        return False

    return True


def perc_down(arr, r, n):
    """Percolate the value at n down the tree.

    invariant: all of n's children must be valid binary heaps.
    """
    max_i = max_family(arr, r, n)
    while max_i != n:
        # node must always be the largest of their family
        swap(arr, n, max_i)

        # keep following that path
        n, max_i = max_i, max_family(arr, r, max_i)


def perc_up(arr, r, n):
    """Percolate a value n from the bottom up the heap.

    invariant: arr in range r must be a valid heap except for n
    """
    p = parent(r, n)
    while p is not None:
        max_i = max_family(arr, r, p)
        if max_i == p:
            # heap condition met
            break
        swap(arr, p, max_i)

        # follow the parent up the tree
        n, p = p, parent(r, p)


def heapify(arr, r=None, debug=False):
    """Heapify an unordered array."""
    if r is None:
        r = 0, len(arr) - 1

    if r[1] - r[0] <= 0:
        # length 1 or 0 array
        return

    li, hi = r
    n = parent(r, hi)  # select the parent of the right-most node

    while n >= li:
        perc_down(arr, r, n)
        n -= 1
        if debug:
            assert is_heap(arr, r, n)


def insert(arr, value):
    """Insert a value into the max-heap."""
    arr.append(value)
    li, hi = 0, len(arr) - 1
    r = li, hi

    # we've appended a value, we must fix the heap by
    # percolating it up
    perc_up(arr, r, hi)


def pop_max(arr):
    """Pop the maximum value of the max-heap and return it."""
    if len(arr) == 1:
        return arr.pop()

    max_val = arr[0]

    # move the last value to the root and fix
    arr[0] = arr.pop()
    li, hi = 0, len(arr) - 1
    r = li, hi

    perc_down(arr, r, li)
    return max_val


def heapsort(arr):
    """Sort the array using heapsort."""
    heapify(arr)

    hi = len(arr) - 1
    while hi >= 1:
        swap(arr, 0, hi)
        hi -= 1
        r = 0, hi
        heapify(arr, r)

################
# TESTS

def test_relationships():
    arr = [0, 1, 2, 3, 4, 5, 6, -1]
    r = 0, len(arr) - 1

    # test basic range
    assert left(r, 0) == 1
    assert right(r, 0) == 2
    assert parent(r, 0) is None
    assert parent(r, 1) == 0
    assert left(r, 1) == 3
    assert right(r, 1) == 4
    assert parent(r, 2) == 0
    assert left(r, 2) == 5
    assert right(r, 2) == 6

    assert left(r, 3) == 7
    assert parent(r, 7) == 3

    assert min_family(arr, r, 0) == 0
    assert max_family(arr, r, 0) == 2
    assert min_family(arr, r, 3) == 7
    assert max_family(arr, r, 3) == 3

    # test slicing
    r = 4, len(arr) - 1
    # [.., 4, 5, 6, -1]
    assert left(r, 4) == 5
    assert right(r, 4) == 6
    assert left(r, 5) == 7
    assert right(r, 5) == 8

    assert parent(r, 4) is None
    assert parent(r, 5) == 4
    assert parent(r, 6) == 4
    assert parent(r, 7) == 5

    assert min_family(arr, r, 4) == 4
    assert max_family(arr, r, 4) == 6
    assert min_family(arr, r, 5) == 7


def test_heap():
    print()
    example = list(range(10))
    r = 0, len(example) - 1

    # test basic is_heap
    assert not is_heap(example, r, 0)
    example.reverse()
    assert is_heap(example, r, 0)

    # test heapify
    res = list(example)
    heapify(res)
    assert res == example, "heapify on valid heap doesn't change it"

    example.reverse()
    res = list(example)
    heapify(res)
    assert is_heap(res, r, 0)
    print("orig:{}\nres :{}".format(example, res))


def test_insert_pop():
    max_range = 10

    arr = list(range(1, max_range + 1))
    print("\nINSERT")
    print("start:", arr)
    heapify(arr)
    assert arr[0] == max_range

    print("heap :", arr)
    max_ins = 100

    insert(arr, max_ins)
    r = 0, len(arr) - 1
    assert arr[0] == max_ins
    assert is_heap(arr, r, 0)
    print("ins  :", arr)

    assert pop_max(arr) == max_ins
    r = 0, len(arr) - 1
    assert is_heap(arr, r, 0)
    print("pop  :", arr)

    small_ins = 2
    assert arr[0] == max_range

    insert(arr, small_ins)
    r = 0, len(arr) - 1
    assert arr[0] == max_range
    assert is_heap(arr, r, 0)
    assert arr.count(small_ins) == 2
    print("ins  :", arr)

    assert pop_max(arr) == 10
    assert pop_max(arr) == 9
    assert pop_max(arr) == 8

    assert len(arr) == max_range - 2


def check_sort(orig):
    print()
    arr = list(orig)
    heapsort(arr)

    print("orig:{}\nsort:{}".format(orig, arr))
    assert arr == sorted(orig)


def test_sort():
    check_sort([4,5,1,2])
    check_sort([8,2,3,5,3,10,2])
    check_sort(list(range(10)))
    check_sort(list(range(100, -1, -1)))
