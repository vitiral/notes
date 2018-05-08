"""PG-51: Trees and Graphs

"""
import copy

class TreeNode(object):
    def __init__(self, val, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

BASIC = TreeNode(
    3,
    left=TreeNode(2),
    right=TreeNode(4),
)

BASIC2 = TreeNode(
    3,
    left=TreeNode(
        2,
        left=TreeNode(1),
        right=None,
    ),
    right=TreeNode(4),
)

UNBALANCED = TreeNode(
    10,
    left=TreeNode(5,
        left=TreeNode(
            2,
            left=TreeNode(1),
            right=None,
        ),
        right=TreeNode(7),
    ),
    right=TreeNode(15),
)

################################################################################
# PG-52: 4.1 Implement a function to check if a tree is balanced.
#
# For the purposes of this question, a balanced tree is defined to be a tree
# such that no two leaf nodes differ in distance from the root by more than one.

"""
Notes:
- Simplest answer I can think of is to get the max and min depths of leaves.
- I _believe_ these could be done simultaniously (walk the tree, tracking both)
  but I would prefer to implement them separately and then merge them.
"""

def is_balanced(tree):
    min_depth, max_depth = min_max_depth(tree, 0)
    return max_depth - min_depth <= 1

def min_max_depth(node, cur_depth) -> (int, int):
    if node is None:
        return (cur_depth, cur_depth)

    cur_depth += 1

    left_min, left_max = min_max_depth(node.left, cur_depth)
    right_min, right_max = min_max_depth(node.right, cur_depth)

    return min(left_min, right_min), max(left_max, right_max)


def max_depth(node, cur_depth):
    if node is None:
        return cur_depth
    cur_depth += 1

    return max(
        max_depth(node.left),
        max_depth(node.right),
    )


def min_depth(node, cur_depth):
    if node is None:
        return cur_depth

    cur_depth += 1
    return min(
        min_depth(node.left),
        min_depth(node.right),
    )


def test_is_balanced():
    assert is_balanced(BASIC)
    assert is_balanced(BASIC2)
    assert not is_balanced(UNBALANCED)
