class Node(object):
    def __init__(self, val, next=None):
        self.val = val
        self.next = next

################################################################################
# PG-48: 2.4 You have two numbers represented by a linked list, where each node
# contains a single digit The digits are stored in reverse order, such that the
# 1’s digit is at the head of the list Write a function that adds the two
# numbers and returns the sum as a linked list
#    EXAMPLE
#       Input: (3 -> 1 -> 5) + (5 -> 9 -> 2)
#       Output: 8 -> 0 -> 8
#
# actual-answer: 102 (says 108... so always subtract 6?)

"""
The first difficulty I notice is what happens when ditis are large.

- (3 -> 1 -> 5) + (5 -> 9 -> 2) = (513 + 295) = 808
- each number can be easily calculated without knowing the width length.
- (3*10^0) + (1*10^1) + (5*10^2) == 513

"""

def ll_sum(left, right):
    return into_to_ll(sum_ll_numbers(left, right))


def int_to_ll(value):
    head = Node(value % 10)
    value = value // 10
    node = head
    while value > 0:
        node.next = Node(value % 10)
        node = node.next
        value = value // 10
    return head


def sum_ll_numbers(left, right):
    return ll_value(left) + ll_value(right)


def ll_value(ll):
    value = 0
    digit = 0
    node = ll
    while node:
        value += node.val * (10 ** digit)
        digit += 1
        node = node.next
    return value

def create_ll(arr):
    if not arr:
        return None
    root = Node(arr[0])
    node = root
    for val in arr[1:]:
        node.next = Node(val)
        node = node.next
    return root

def test_ll_value():
    assert ll_value(create_ll([])) == 0
    assert ll_value(create_ll([0])) == 0
    assert ll_value(create_ll([1])) == 1
    assert ll_value(create_ll([1, 0])) == 1
    assert ll_value(create_ll([0, 1])) == 10
    assert ll_value(create_ll([4,2,1])) == 124


def test_sum_ll():
    result = sum_ll_numbers(
        create_ll([1]), create_ll([0, 1])
    )
    assert result == 11
    # assert int_to_ll(result) == create_ll([1, 1])

    result = sum_ll_numbers(
        create_ll([4,2,1]), create_ll([0, 1])
    )
    assert result == 134
    result = int_to_ll(result)
    expected = create_ll([4, 3, 1])
    while result:
        assert result.val == expected.val
        result = result.next
        expected = expected.next
        if result is None or expected is None:
            assert expected is None, "lenghts are different"
            assert result is None, "lenghts are different"
        else:
            assert result.val == expected.val


################################################################################
# PG-48 2.5: Given a circular linked list, implement an algorithm which returns node
# at the beginning of the loop
#
# DEFINITION
# Circular linked list: A (corrupt) linked list in which a node’s next pointer
# points to an earlier node, so as to make a loop in the linked list
#
# EXAMPLE
# input: A -> B -> C -> D -> E -> C [the same C as earlier]
# output: C
#
# Okay, I recall that problems like this can typically be solved
# by "walking" the list with two walkers, a fast and a slow, but
# I can't fully recall the details.
#
# % = slow, ^ =fast
# Case1: loop over example
# A   B   C   D   E   C
# %^
#
# A   B   C   D   E   C
#     %   ^
#
# A   B   C   D   E   C
#         %       ^
#
# Conclusion Case1: They met!
# A   B   C   D   E   C
#             ^%

# Case 1 cont..
# A   B   C   D   E   C
#             ^%
#
# A   B   C   D   E   C
#         ^       %

# A   B   C   D   E   C
#         %       ^

# Case1 cont: they meet again after 3 steps
# loop is length=3
# A   B   C   D   E   C
#             %^
#
#
# Case 2: start at below, note that we link to D instead
# A   B   C   D   E   D
#         %       ^
#
# A   B   C   D   E   D
#         %       ^
#
# A   B   C   D   E   D
#             %   ^
#
# Conclusion Case2: They met!
# A   B   C   D   E   D
#             ^%
#
# Case2 cont... 2 more loop to meet again.
# loop = length 2
# A   B   C   D   E   D
#             ^   %
#
# Case 3: start at below, note that we link to D instead
# A   B   C   D   E   F   G   C
#         %       ^
#
# A   B   C   D   E   F   G   C
#             %       ^
#
# A   B   C   D   E   F   G   C
#         ^       %
#
# A   B   C   D   E   F   G   C
#                 ^   %
#
# Conclusion: they meet!
# A   B   C   D   E   F   G   C
#                         %^
#
# Case2 cont...
# A   B   C   D   E   F   G   C
#         %   ^
#
# A   B   C   D   E   F   G   C
#             %       ^
#
# A   B   C   D   E   F   G   C
#             %       ^
#
# A   B   C   D   E   F   G   C
#         ^       %

# A   B   C   D   E   F   G   C
#                 ^   %
#
# So: walk the list with two walkers, a fast and a slow.
# - if the list completes, return that there are no loops.
# - if the walkers meet eachother, return where they meet.
# - to find where the loop starts: set the location of one
#   the head (and keep the location of the other at the
#   meeting point) and increment both by one until they meet.
#   This works because MAGIC
