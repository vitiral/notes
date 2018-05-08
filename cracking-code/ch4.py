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


################################################################################
# pg-52: 4.2 given a directed graph, design an algorithm to find out whether
# there is a route between two nodes

def route_exists(a, b):
    """Determine if there is a route between node a and node b.

    This algorithm will run indefinitely if there is a loop in the graph.
    We could mark each node as "already visited" to avoid this problem.
    """
    if a is b:
        return True

    for child in a.children:
        if route_exists(child, b):
            return True
    return False

################################################################################
# pg-52:  4.3 Given a sorted (increasing order) array, write an algorithm to
# create a binary tree with minimal height.

"""
Array is _already_ a binary tree with minimal height if left=(i*2)+1 right=(i*2)+2

Minimum height = log_2(len(arr))

Inorder traversal of a binary tree gives you a sorted array
- left
- self
- right

Constructing via inorder traversal is _probably_ best.
No...

We know several things:
- the middle value should be the root
- the values to the left are on the left
- the values to the right are on the right

[1, 2, 3, 4, 5, 6, 7, 8, 9]

          ---5---
         /       \*
     [1,2,3,4] [6,7,8,9]
     2              7
    / \            / \*
    1  [3,4]      6  [8,9]
        \*              \*
         3               8
          \               \*
           4               9


      ---5---
     /       \*
    2         7
   / \       / \*
  1   3     6   8
       \         \*
        4         9

"""
def construct_bst_from_sorted(sorted_array, li, hi):
    _from_sorted(sorted_array, 0, len(sorted_array) - 1)

def _from_sorted(sorted_array, li, hi):
    if hi - li < 0:
        return None

    mi = (hi - li) // 2
    return TreeNode(
        sorted_array[mi],
        left=_from_sorted(sorted_array, li, mi-1),
        right=_from_sorted(sorted_array, mi+1, hi),
    )

################################################################################
# pg-52: 4.5 Write an algorithm to find the ‘next’ node (i.e., in-order
# successor) of a given node in a binary search tree where each node has a link
# to its parent


