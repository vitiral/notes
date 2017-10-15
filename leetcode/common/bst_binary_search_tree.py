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
