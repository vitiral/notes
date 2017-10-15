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

    assert parent(r, 7) == 3

    # test slicing
    r = 4, len(arr) - 1
    assert left(r, 4) == 5
    assert right(r, 4) == 6
    assert left(r, 5) == 7
    assert right(r, 5) == 8

    assert parent(r, 4) is None
    assert parent(r, 5) == 4
    assert parent(r, 6) == 4
    assert parent(r, 7) == 5

    # hi = len(arr) - 1
    # assert min_family(arr, 0, hi) == 0
    # assert min_family(arr, 1, hi) == 1
    # assert min_family(arr, 3, hi) == 7  # index 7 value == -1

    # # parent using min
    # assert parent(0, 7) == 3
    # assert parent(4, 7) is None

    # arr = [0, 1, 2, 3, 4, 5, 6, 8, 9, 7]
    # assert max_family(arr, 7, 9) == 8
