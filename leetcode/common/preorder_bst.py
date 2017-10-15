"""Given preorder traversal of a binary search tree, construct the BST."""

class PreorderPos:
    data = None
    index = None

class Node(object):
    def __init__(self, key, left=None, right=None):
        self.key = key
        self.left = left
        self.right = right

    @classmethod
    def from_preorder(cls, preorder):
        pos = PreorderPos
        pos.data = preorder
        pos.index = 0
        return cls._from_preorder(pos)

    @staticmethod
    def _from_preorder(pos, parent_key=None, is_left=False):
        """Get BST from preorder.

        :param iter[int] preorder: an iterator that we consume
        :rtype: Node
        """
        if pos.index >= len(pos.data):
            return None

        key = pos.data[pos.index]
        if parent_key is None:
            # root index
            assert pos.index == 0
            node = Node(key)
        elif is_left:
            if key < parent_key:
                # we are valid left
                node = Node(key)
            else:
                # we are INVALID left, this must be a right
                # DON'T consume an index
                return None
        else:
            raise NotImplementedError()

        # we have to build our own left and right
        pos.index += 1  # look at the next index
        node.left = Node._from_preorder(pos, key, is_left=True)
        if node.left is None:
            # We tried to set it but it failed, it is refering to
            # the right of a higher node.
            return node
        node.right = Node._from_preorder(pos, key, is_left=False)

        return node

def preorder(node):
    """Return the preorder of the node."""
    out = []
    _preorder(out, node)
    return out

def _preorder(preorder, node):
    if node is None:
        return

    preorder.append(node.key)
    _preorder(preorder, node.left)
    _preorder(preorder, node.right)

def test_example():
    '''
    Example:
         10
       /   \
      5     40
     /  \      \
    1    7      50
    '''
    example = [10, 5, 1, 7, 40, 50]
    expected = Node(10)
    expected.left = Node(5)
    expected.left.left = Node(1)
    expected.left.right = Node(7)
    expected.right = Node(40)
    expected.right.right = Node(50)

    assert preorder(expected) == example
    assert preorder(Node.from_preorder(example)) == example

