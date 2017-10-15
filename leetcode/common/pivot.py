# Search an element in a sorted and rotated array

# An element in a sorted array can be found in O(log n) time via binary search.
# But suppose we rotate an ascending order sorted array at some pivot unknown
# to you beforehand. So for instance, 1 2 3 4 5 might become 3 4 5 1 2.
# Devise a way to find an element in the rotated array in O(log n) time.

input = [5, 6, 7, 8, 9, 10, 1, 2, 3]
key = 3
expected = 8

input = [5, 6, 7, 8, 9, 10, 1, 2, 3]
key = 30
expected = None

input = [30, 40, 50, 10, 20]
key = 10
expected = 3


# normal binary search picks the middle and compares the value there to
# the key and eliminates half
#
# They want us to achieve the same performance in the same time.
# It is tempting to say this is impossible...
#
# The first thing to note is that everything is *still* in asending order
# until the piviot
#
# THEORY:
# The piviot is pretty much irrelevant if you just wrap the array.
#
# Example:
# key=3, input = [5, 6, 1, 2, 3], key=3
#
# The piviot essentially requires us to do 1 extra check, so instead
# of N(log(n)) it's technically N(log(n) + 1) -- however such additions
# are irrelevant in those expressions.
#
# THEORY:
# If we start at the end of the array and find that values are less,
# we can compare to the middle. If those values are more we know
# the piviot is somewhere in between but we also know our VALUE
# is somewhere in between.
#
# So just continue searching like we normally would in a BS, go to len * 3/4
# If we ever find a value that the key is less than we go up,
# If we find a value that the key is greater than we do another extra check:
# we check to see if the middle...
#
# God this is hard... unless we can first find the piviot point, which I was
# starting to think was the answer. It makes sense, that is technically
# O(2 * log n) time, but that is actually abbreviated as O(log n) time.
#
# So ya, we find the piviot by:
# - start at the middle and record the value
# - look to the left-middle, if it is > tmp, the piviot is to the right
#   if it is < tmp, the piviot is to the right of TMP

def find_piviot(data, start, end, last_mid=None, is_left=False):
    """Find the piviot point.

    :param list data: data to search.
    :param int start: starting index to use.
    :param int end: ending index to use.

    :returns: None if the piviot is not in this range,
        else the piviot index
    :rtype: None | int
    """
    if last_mid is None:
        # this is the first call, just initialize and do left search
        is_left = True
        end = start + (end - start) // 2
        last_mid = data[end]

    if end - start == 1:
        # this is the piviot, horray!
        return start

    # get the middle of the requested range
    mid_i = start + (end - start) // 2
    mid = data[mid_i]

    if is_left and mid > last_mid:
        # the piviot must be to the right
        piviot = find_piviot(
            data, start=mid_i, end=end, last_mid=mid, is_left=False
        )
        if piviot is None:
            # left was the wrong direction, look right instead

    elif is_left and mid < last_mid:
        # the piviot must be to the right of TMP,
        # we are in completely the wrong section
        return None
