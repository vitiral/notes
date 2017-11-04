"""Heapsort is a O(n log n) best+worst+average case sorting algorithm.

It is an in-place sort that uses binary heaps.

First, the array is represented as an unsorted but **complete** Binary Tree by
deciding that:
- each node is at index `k`
- the left child of each node is at index `k*2 + 1`
- the right child of each node is at index `k*2 + 2`
- the parent of any node is `(k-1)/2`

```
data = [A, B, C, D, E]
# A has children B=2*0+1 & C=0*2+2
# B has child D=2*1+1 & E=2*1+2 and parent A=(1-1)/2
# C has no children, and parent A=(2-1)/2=1/2=0
```

> A complete binary tree is a binary tree (parent with two child nodes) that
> is completely filled except the last row which is filled to the left.

This takes no time as we are just representing the array as a binary tree (cool!).

We then need to turn the Binary Tree into a max-heap Binary Heap. We do this
in-place by starting at the last node and "bubbling" it up to its parent node
for ALL nodes.

> A binary heap is a Binary Tree with ONE OF the following properties for ALL
> nodes:
> - min-heap: the value of the parent is less than the child nodes
> - max-heap: the value of the parent is greater than both child nodes
"""

def swap(arr, i, j):
    """Swap two indexes in an array."""
    arr[i], arr[j] = arr[j], arr[i]


def left(root_i):
     return 2 * root_i + 1

def right(root_i):
    return 2 * root_i + 2


def max_heapify(arr, root_i, max_i):
    """Heapify (to a max-heap) an array from root_i (the root index) to max_i.

    invariant: this assumes that all nodes with indexes > root_i
        have already been heapified, so we are just moving any small
        data found at root_i FROM root_i DOWN the heap.

    Basically this percolates-down the value at root_i.
    """
    # find the largest index by first assuming it is at root
    largest_i = root_i
    left_i = left(root_i)
    right_i = right(root_i)

    # we want to find the largest index of the {root,left,right}
    # that should be the new root
    if left_i <= max_i and arr[left_i] > arr[largest_i]:
        largest_i = left_i

    if right_i <= max_i and arr[right_i] > arr[largest_i]:
        largest_i = right_i

    if largest_i != root_i:
        swap(arr, largest_i, root_i)

        # heapify using the new root as largest_i
        max_heapify(arr, largest_i, max_i)

def heapsort(arr, debug=False):
    """Sort the array using heapsort."""

    max_i = len(arr) - 1

    # heapify the array from back to front.
    # the max_heapify function has an invariant that requires
    # that all sub-nodes have already been heapified.
    for root_i in range(max_i, -1, -1):
        max_heapify(arr, root_i, max_i)
        if debug:
            check_heap(arr, root_i, max_i)

    # One by one extract elements.
    #
    # Because of the property of the max-heap, we know that the largest element
    # will always be at the root index (index 0) so we put that value at the
    # end of the array and then fix the
    # heap.
    #
    # We are overriding max_i because effectively the length of the array
    # we are interested in is "shrinking" until it is only 1 item long.
    for max_i in range(max_i, 0, -1):
        # put the root of the heap (which is always the max value)
        # at the moving-end
        swap(arr, 0, max_i)

        # fix the shrunken heap that we have just messeed with
        max_heapify(arr, 0, max_i-1)
        if debug:
            check_heap(arr, 0, max_i-1)

# ------
# - TESTS

def check_heap(arr, root_i, max_i):
    # make sure that the heap is a valid heap
    for root_i in range(root_i, max_i + 1):
        root = arr[root_i]

        left_i = left(root_i)
        right_i = right(root_i)

        if left_i <= max_i and arr[left_i] > root:
            return False

        if right_i <= max_i and arr[right_i] > root:
            return False

    return True

def check_sort(orig):
    print()
    arr = list(orig)
    heapsort(arr, debug=True)

    print("orig:{}\nsort:{}".format(orig, arr))
    assert arr == sorted(orig)

def test_sort():
    check_sort([4,5,1,2])
    check_sort([8,2,3,5,3,10,2])
    check_sort(list(range(10)))
