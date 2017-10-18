"""Given two numbers represented by linked lists (largest digit is root), add
the numbers together. Not allowed to use explicit extra space (!!!).

Hint: use recursion

LSN == Least Significant Node
MSN == Most Significant Node

These lists are in MSN -> LSN format.

It feels obvious that we have to add the numbers from LSB -> MSB and then add
the overflow to the next highest values, etc. However, the lists are
no arranged in a pleasant way to do so.

Unfortunately, this is not how the data is arranged!

Agh, this is quite frustrating! Some magical recursion clearly must be
done.


   [7, 8, 9]  # 2
[1, 3, 4, 5]  # 3

left-digit-count
 0  1  2  3

"""

class Node(object):
    def __init__(self, data, lesser_node=None):
        self.data = data

        # a node that has data of a lesser signficant value
        self.lesser_node = lesser_node


def get_lsd(msd_count, msd_max):
    """
    3 digit number: 4 5 6

    0 2: 2
    1 2: 1
    2 2: 0
    """
    return msd_max - msd_count


def add(v1, v2, v1_msd_count=0, v2_msd_count=0):
    """Add two linked list values together.

    :returns: (Node, max_v1_digit_count, max_v2_digit_count)
    """
    # stack space is always allowed :)


    if v1.lesser_node:
        node, v1_max, v2_max = add(v1.lesser_node, v2, v1_msd_count+=1, v2_msd_count)
    else:
        if v2.lesser_node:
            node, v1_max, v2_max = add(v1, v2.lesser_node, v1_msd_count, v2_msd_count+=1)
        else:
            # know that we are at the lowest point, horray
            val = v1.data + v2.data

            lsv = val % 10
            msv = val // 10
            node = Node(lsv)
            if msv:
                node = Node(msv, lesser_node=node)
            return node, v1_msd_count, v2_msd_count

    # Do calculations and logic
    v1_lsd = get_lsd(v1_msd_count, v1_max)
    v2_lsd = get_lsd(v2_msd_count, v2_max)
    if v1_lsd == v2_lsd:
        # TODO: we do our calculations and return
        return node, v1_max, v2_max
    else:
        return node, v1_max, v2_max

    # we now know the longest digit value and can just
    # keep adding onto it.


