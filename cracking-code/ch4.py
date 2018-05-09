"""PG-51: Trees and Graphs

"""
import copy

class TreeNode(object):
    def __init__(self, val, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    def __repr__(self):
        return "<{}>".format(self.val)

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

"""
Notes:

In order traversal is:
- left
- self
- right

So lets start from the root and work our way through.
- first we return the left most node
- then, knowing only that node, we should return it's parent
- then, knowing only that node, we should return it's right node

To "kick off" the traversal we would be _given_ the left_most_node
and asked to find the successor.

"""


def successor(node):
    if node is None:
        return None

    if node.right:
        return left_most_node(node.right)

    return find_parent_where_self_is_left(node)


def find_parent_where_self_is_left(node):
    if node is None or node.parent is None:
        return None

    if node.parent.left is node:
        return node.parent

    return find_parent_where_self_is_left(node.parent)


def left_most_node(node):
    if node is None:
        return None
    while node.left:
        node = node.left
    return node


def set_parents(node, parent=None):
    node.parent = parent
    if node.left:
        set_parents(node.left, parent=node)
    if node.right:
        set_parents(node.right, parent=node)

def test_successor():
    basic = copy.deepcopy(BASIC)
    set_parents(basic)
    assert left_most_node(basic) is basic.left
    assert successor(basic.left) is basic
    assert successor(basic) is basic.right
    assert successor(basic.right) is None


################################################################################
# PG-52 4.7 You have two very large binary trees: T1, with millions of
# nodes, and T2, with hundreds of nodes.

# Create an algorithm to decide if T2 is a subtree of T1

"""
Obviously in this case we cannot use depth-first because of the memory
requirements.

You can compare two trees using pre-order traversal, so a brute force
solution is to do this operation for every possible node in T2.

I _want_ to use hashing for this. If T2 had the hash of all of it's
trees pre-computed then we could simply ask if the hash of T1 existed
in T2. Hashing takes only O(n) (since the hash of each node can be
something like hash(hash(node.val) + hash(node.left) + hash(node.right)).
But the hash of left and right can be pre-computed and cached.

Solution:
- you can use preorder and inorder traversal for this.
  - If T2's (preorder/postorder) is a subset of T1's (preorder/postorder)
    then T1 is a subset of T2. You can check this using a suffix tree TODO: wut?
"""

def trees_equal(t1, t2):
    if t1 is None:
        if t2 is None:
            return True
        else:
            return False

    if t1.val != t2.val:
        return False

    if not trees_equal(t1.left, t2.left):
        return False

    if not trees_equal(t1.right, t2.right):
        return False
