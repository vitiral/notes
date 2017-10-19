"""Detect if a binary tree is a BST

from memory: a binary search tree is defined as a binary
tree which has the properties:
- the leaves of each node is less than or equal to the node (or vice versa)
- ... no this is wrong.

The actual definition is:
- the left subnode's data is always less than the node's data
- the right subnode's data is always greater than the node's data
- both left+right must also be BST

Therefore:
- each node has a distinct data


Self balancing binary tree: the AVL Tree
If we are inserting `I*`

Left-Left

        uG_
       /   \
      U*    uU
     / \     \
   uC   oC    X
   /
  gC
 /
I*


Left-Right

        uG_
       /   \
      U*    uU
     / \     \
   uC   oC    X
    \
     gC
      \
       I*

1. Perform standard BST insert for `I*`
2. Starting from `I*`
    - Travel up and find the first unbalanced node `U`
    - Let `uC` be the child of `U` that comes on the path from `I*` to `U`
    - Let `gC` be the grandchild of `U` that comes on the path from `I* to `U`
3. Re-balance the tree by performing appropriate rotations on the subtree
   rooted with `U`. There can be 4 possible cases that needs to be handled
    a) both the child (uC) and grandchild (gC) of U go left (Left Left Case)
    b) the child (uC) is left, but uG is right of uC (Left Right Case)
    c) both the child (uC) and grandchild (gC) of U go right (Right Right Case)
    d) the child (uC) is right, but uG is left of uC (Right Left Case)

To rebalance in each case:
- Left-Left: simply rotate U right
- Left-Right: roate uC left, then rotate U right
- Right-Right: rotate U left
- Right-Left: rotate uC right, then rotate U left

After any rotation, the heights are updated via:
- left.height = max(height(left.left), height(left.right)) + 1;
- right.height = max(height(right.left), height(right.right)) + 1;

It *seems* like we should be able to know the height from rotations
alone, but alas that seems non-trivial
"""

INT_MAX = 4294967296
INT_MIN = -4294967296

class Node(object):
    def __init__(self, data, left=None, right=None):
        self.data = data
        self.left = left
        self.right = right

    def is_bst(self, minval=INT_MIN, maxval=INT_MAX):
        """Traverse the binary tree in order, narrowing the available values as we go."""
        if self.data >= maxval:
            # print("maxval", maxval, ": failed at", self.data)
            return False
        if self.data <= minval:
            # print("minval",  minval, ": failed at", self.data)
            return False

        # otherwise check the subtrees recursively with
        # updated minvals and maxvals based on own data
        if self.left is not None and not self.left.is_bst(minval=minval, maxval=self.data):
            # print("left: failed at", self.data)
            return False
        elif self.right is not None and not self.right.is_bst(minval=self.data, maxval=maxval):
            # print("right: failed at", self.data)
            return False
        else:
            return True

    def is_bst_WRONG(self):
        if self.left is not None and self.left.data > self.data:
            return False
        elif self.right is not None and self.right.data < self.data:
            return False
        elif self.left is not None and not self.left.is_bst():
            return False
        elif self.right is not None and not self.right.is_bst():
            return False
        else:
            return True


# create a basic BST for testing
bst = Node(10)
bst.left = Node(8)
bst.right = Node(12)
bst.right.right = Node(14)

assert bst.is_bst()

# create a NON BST for testing
btree = Node(10)
btree.left = Node(8)
btree.right = Node(12)
btree.right.left = Node(7)

assert not btree.is_bst()
