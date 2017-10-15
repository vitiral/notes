

def mergesort(arr):
    """Sort using mergesort.
    """
    if len(arr) <= 1:
        return
    cache = list(range(len(arr)))

    if mergesort_helper(False, arr, cache, 0, len(arr) - 1):
        # copy data over, since it didn't use the arr as cache
        for i in range(len(arr)):
            arr[i] = cache[i]


def mergesort_helper(a_is_cache, A, B, low, hi):
    """Helper function for implementing mergesort.

    Sort the arr[low:hi+1] range in place.

    :param list arr: the arr values
    :param list out: place to store output values
    :param int low: lowest index to use
    :param int hi: highest index to use

    :returns: a_is_cache
    :rtype: bool
    """
    if hi == low:
        # array of len==1
        return a_is_cache

    if a_is_cache:
        arr, cache = B, A
    else:
        arr, cache = A, B

    # length=2 array has hi==1 1 // 2 == 0, which is correct mid
    # length=4 array has hi==3 3 // 2 == 1, which is correct mid
    # length=5 array has hi==4 4 // 2 == 2, whch is correct mid
    mid = (hi - low) // 2

    mergesort_helper(arr, cache, low, mid)
    mergesort_helper(arr, cache, mid+1, hi)

    merge(cache, arr, low, mid, mid+1, hi)
    return not a_is_cache


def merge(inp, out, low_a, hi_a, low_b, hi_b):
    """Merge two equally sized SORTED arrays.

    Merge is performed by continually incremeting
    low_a and low_b when the lesser value is
    found until both arrays are exahusted.

    Any data left in either is already sorted, so it
    is just drained directly to the output.
    """
    i = low_a
    while low_a <= hi_a and low_b <= hi_b:
        if inp[low_a] < inp[hi_b]:
            # low_a is less
            out[i] = inp[low_a]
            low_a += 1
        else:
            # low_b is less
            out[i] = inp[low_b]
            low_b += 1
        i += 1

    # drain any remaining data
    while low_a <= hi_a:
        out[i] = inp[low_a]
        low_a += 1
        i += 1

    while low_b <= hi_b:
        out[i] = inp[low_b]
        low_b += 1
        i += 1

