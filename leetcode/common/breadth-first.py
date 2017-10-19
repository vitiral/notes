# breadth first traversal of a tree
from collections import deque

class Node(object):
    def __init__(self, key, left=None, right=None):
        self.key = key
        self.left = left
        self.right = right

    def __repr__(self):
        return "Node<{}>".format(self.key)


def traverse_levelorder(node):
    """Returns an array-of-arrays of the tree, where each index is
    a single "level"
    """
    if node is None:
        return

    out = []
    def visit(level, node):
        if level > len(out) - 1:
            out.append([])
        out[level].append(node.key)

    # we use q as just a regular q
    q = deque()
    # q holds the current level and node
    q.appendleft((0, node))
    while q:
        level, node = q.pop()
        visit(level, node)
        if node.left is not None:
            q.appendleft((level+1, node.left))
        if node.right is not None:
            q.appendleft((level+1, node.right))

    return out

example = Node(
    22,
    Node(8, Node(4), Node(20)),
    Node(50, Node(30), Node(70)),
)

levels = traverse_levelorder(example)

expected = [
    [22],
    [8, 50],
    [4, 20, 30, 70],
]

print(levels)
assert levels == expected
