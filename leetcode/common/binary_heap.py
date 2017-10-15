"""Methods of a binary heap represented on a zero-indexed array.
"""

def swap(arr, i, j):
    """Swap two indexes in array."""
    arr[i], arr[j] = arr[j], arr[i]


def left(li, i):
    """The left node of the index."""
    return 2 * (i - li) + 1 + li


def right(li, i):
    """The left node of the index."""
    return 2 * (i - li) + 2 + li


def parent(li, i):
    """The parent of the index."""
    out = (i - 1) // 2
    if out < li:
        return None
    return out


def min_family(arr, i, hi):
    """Return the index which has the minimum value of the family.

    If the minimum exists on i and another node, i is returned.
    """
    left_i = left(i, i)
    if left_i > hi:
        # no children
        return i

    right_i = right(i, i)
    min_i = i

    # already checked index
    if arr[left_i] < arr[min_i]:
        min_i = left_i

    if right_i <= hi and arr[right_i] < arr[min_i]:
        min_i = right_i

    return min_i


def max_family(arr, i, hi):
    """Return the index which has the maximum value of the family.

    If the maximum exists on i and another node, i is returned.
    """
    left_i = left(i, i)
    if left_i > hi:
        # no children
        return i

    right_i = right(i, i)
    max_i = i

    # already checked left index
    if arr[left_i] > arr[max_i]:
        max_i = left_i

    if right_i <= hi and arr[right_i] > arr[max_i]:
        max_i = right_i

    return max_i


def test_relationships():
    arr = [0, 1, 2, 3, 4, 5, 6, -1]

    assert left(0, 0) == 1
    assert right(0, 0) == 2
    assert parent(0, 0) is None

    assert parent(0, 1) == 0
    assert left(0, 1) == 3
    assert right(0, 1) == 4

    assert parent(0, 2) == 0
    assert left(0, 2) == 5
    assert right(0, 2) == 6

    assert parent(0, 7) == 3

    hi = len(arr) - 1
    assert min_family(arr, 0, hi) == 0
    assert min_family(arr, 1, hi) == 1
    assert min_family(arr, 3, hi) == 7  # index 7 value == -1

    # parent using min
    assert parent(0, 7) == 3
    assert parent(4, 7) is None

    arr = [0, 1, 2, 3, 4, 5, 6, 8, 9, 7]
    assert max_family(arr, 7, 9) == 8

def percolate_up(arr, li, i):
    """max-heap: Bubble the value at index `i` up.

    The heap from arr[li:i+1] must be valid except for the value at `i`.
    """
    p = parent(i, li=li)
    print("perc-up: p={}, li={}, i={}".format(p, li, i))
    while p is not None:
        if arr[p] < arr[i]:
            # parent is less than the child, which needs to change
            swap(arr, p, i)
        else:
            break

        # Follow the value up the tree.
        i, p = p, parent(i)


def percolate_down(arr, i, hi):
    """max-heap: Bubble the value at index `i` down.

    The heap must be valid except for the value at `i`.
    """
    while left(i, i) < hi:  # while i has children
        # begin loop invariant: i always references the same VALUE
        max_i = max_family(i)
        if max_i == i:
            # the maximum value is the parent, we are done
            break
        # make the maximum index the new parent
        # and continue to follow the value down
        swap(arr, i, max_i)
        i = max_i


def is_heap(arr, li=0, hi=None):
    """Return whether the array from [li hi] is a valid max-heap.
    """
    if hi is None:
        hi = len(arr) - 1

    if li >= hi:
        # trivial case
        return True

    i = li
    if max_family(arr, i, hi) != i:
        return False

    # recursively check the children
    if not is_heap(arr, left(li, i), hi):
        return False
    elif not is_heap(arr, right(li, i), hi):
        return False

    return True

def test_is_heap():
    assert is_heap([])
    assert is_heap([1])
    assert is_heap([2, 1])
    assert not is_heap([1,2])
    assert is_heap([6, 5, 3, 2, 1, 1, 1])
    assert not is_heap([5, 6, 4, 2, 1, 1, 1])
    assert not is_heap([8, 9, 7])

def heapify(arr, debug=False):
    """Convert an unsorted array into a binary max-heap."""

    # Percolate-up requires that arr[li:i] is a valid heap
    # (except i itself) so just keep fulfilling that requirement

    # we don't need to heapify the end
    hi = len(arr) - 1
    li = hi - 1
    while li >= 0:
        # invariant: we have already heapified [li+1 hi] (inclusive)
        if debug:
            slc = arr[li+1:hi+1]
            if slc == [8, 9, 7]:
                import pdb; pdb.set_trace()
            print("cur heap: li={} hi={} heap={}".format(li, hi, slc))
            assert is_heap(arr, li+1, hi)

        # make the unchecked value the final leaf node
        # (this is almost exactly the same as an insert!)
        swap(arr, li, hi)

        # fix the heap by percolating that node up as needed.
        percolate_up(arr, li, hi)
        li -= 1


def insert(arr, val):
    """max-heap: Insert a value into the binary heap."""
    val_i = len(arr)
    arr.append(val)
    # fix the heap by percolating the value up the heap
    percolate_up(arr, 0, val_i)


def pop_max(arr, val):
    """max-heap: Remove and return the maximum value of the binary heap."""
    if len(arr) <= 1:
        # we will be left with 0 nodes
        return arr.pop()

    # by definition of max-heap, max value is the root
    max_val = arr[0]
    # move the last value to be the root
    arr[0] = arr.pop()
    # fix the array
    percolate_down(arr, 0, len(arr) - 1)

    return max_val


def test_heapify():
    example = list(range(10))  # sorted list is opposite of max-heap
    print("start  :", example)
    assert not is_heap(example, 0, len(example) - 1)

    heapify(example, debug=True)
    print("heapify:", example)
    assert is_heap(example, 0, len(example) - 1)
