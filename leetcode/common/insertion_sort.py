
def swap(arr, i, j):
    arr[i], arr[j] = arr[j], arr[i]

def insertion_sort(arr):
    """Sort the array using insertion sort.

    Grow the "known sorted" list, starting
    at a list of length == 2.

    Each time the list is grown, move the
    value backwards until it shouldn't
    be moved anymore
    """
    for end in range(1, len(arr)):
        # end is end of the array we are currently sorting
        for j in range(end, 0, -1):
            # j is the index of the value we move backwards
            if arr[j] > arr[j-1]:
                # the sorting condition is met
                break
            swap(arr, j, j - 1)


def insertion_sort_opt(arr):
    """Optimize to remove the swap and use only a single assignment
    in the while loop.
    """
    for end in range(1, len(arr)):
        # loop invariant: arr[:end] is already sorted

        # end is the end of the array we are storting
        # end_val is the value we will move into it's
        # correct place, once we find it
        end_val = arr[end]

        # j is what we compare to the value we are moving
        # j+1 is the location of the space we can put a
        # value
        j = end - 1
        while j >= 0 and arr[j] > end_val:
            arr[j+1] = arr[j]
            j -= 1
        arr[j+1] = end_val


def check_sort(orig):
    print()
    arr = list(orig)
    # insertion_sort(arr)
    insertion_sort_opt(arr)

    print("orig:{}\nsort:{}".format(orig, arr))
    assert arr == sorted(orig)

def test_sort():
    check_sort([4,5,1,2])
    check_sort([8,2,3,5,3,10,2])
    check_sort(list(range(10)))
