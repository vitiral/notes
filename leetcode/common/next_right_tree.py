"""

Input Tree
       A
      / \
     B   C
    / \   \
   D   E   F


Output Tree
       A--->NULL
      / \
     B-->C-->NULL
    / \   \
   D-->E-->F-->NULL

[    [ A ],
    [ B, C ],
  [D, E, _, F],
]

Rotate Tree

       D
      / \
     E   B
    /   / \
   F   C   A


Inorder traversal of input:  [D B E A C F]
Preorder traversal of input: [A B D E C F]  -> B A F C E D
Postordr traversal of input: [D E B C F A]

Inorder traversal of rotate: [F E D C B A]  -> B A F E D C

"""

## This is actually a different problem -- that one should
## be solved by depth-first traversal using a queue

class Node(object):
    def __init__(self, key, left=None, right=None):
        self.key = key
        self.left = left
        self.right = right


good_bst = Node(
    5,
    Node(
        3,
        Node(2),
        Node(4),
    ),
    Node(6),
)

bad_bst = Node(
    3,
    Node(
        2,
        Node(1),
        Node(4),  # this shouldn't be > 3
    ),
    Node(5),
)

def is_bst(node):
    """Wrapper function for calling is_bst_helper"""
    if node is None:
        return True

    try:
        is_bst_helper(None, node)
        return True
    except TypeError:
        return False


def is_bst_helper(val, node):
    """Returns the final value after doing a "traversal".

    Raises TypeError if it detects that this is not a BST.
    """

    # traverse the left
    if node.left is not None:
        val = is_bst_helper(val, node.left)

    # traverse "self"
    if val is not None and val >= node.key:
        raise TypeError()
    val = node.key

    # traverse the right and return "last traversed value"
    if node.right is None:
        return val
    else:
        val = is_bst_helper(val, node.right)
        if val <= node.key:
            raise TypeError()
        return val

assert is_bst(good_bst)
assert not is_bst(bad_bst)

