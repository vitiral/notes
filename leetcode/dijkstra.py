"""Dijkstra's Algorithm.

Given a weighted graph, find the shortest paths to all nodes.

1. Initialize a set of all nodes. Give all nodes the value of infinity.
2. Initialize an empty set of all visited nodes.
3. Give the starting node a value of 0 and visit it
4. Remove the visited node from the set of all nodes.
5. Iterate through it's children
  - new_value = node.value+edge
  - if new_value < child.value: continue
  - child.value = new_value
  - if child in unvisited: visit(child) (go to 4)


# Alternative Algorithm: A star (A*)
Very similar to dijkstra's except that we are given a pre-defined heuristic function.

This algorithm visits each node a maximum of one time, using the heuristic weight
to determine the next node to query.

- Only "neighbor" nodes of the starting node (etc etc) are queried.
- The operation is performed only while discovered nodes are unvisited (to
  avoid non-connected graphs)
- The "next node" to look at is determined by a minheap of the heurstic set.
- The heurstic value is determined by `cost_from_start + heurstic_cost(self, goal)`
"""
import copy

INF = float("inf")


class Node(object):
    def __init__(self, children=None):
        self.children = [] if children is None else children
        self.value = INF
        self.path = []

    def update(self, parent, edge):
        """Perform the node update.

        Return True if self.value was changed.
        """
        value = parent.value + edge
        if value < self.value:
            self.value = value
            self.path = copy.copy(parent.path)
            self.path.append(self)
            return True
        else:
            return False


class Visiting(object):
    """The visiting state, i.e. which nodes are visited/unvisited.

    The `visit` method is used to implement Dijkstra's algorithm.
    """
    def __init__(self, unvisited):
        self.unvisited = unvisited
        self.visited = {}

    def visit(self, node, parent, edge):
        nid = id(node)
        if not node.update(parent, edge) and nid in self.visited:
            # no update and already visited
            return
        self.mark_visited(nid, node)

        for (edge, child) in node.children:
            self.visit(child, node, edge)

    def mark_visited(self, nid, node):
        if nid in self.unvisited:
            self.visited[nid] = node
            del self.unvisited[nid]



# Acts as the dummy starting "parent" node.
STARTING_NODE = Node()
STARTING_NODE.value = 0


def shortest_path(starting):
    """Find the shortest path to all items in the graph, using the starting
    node as the start.

    This method mutates the input values.

    Returns the {id: Node} dictionary of the algorithm computed.
    """
    visiting = Visiting(get_nodes(starting))
    visiting.visit(starting, STARTING_NODE, 0)
    return visiting.visited


def get_nodes(root) -> "Dict[id,Node]":
    """Get all reachable nodes in the graph using a depth-first search."""
    out = {}
    _get_nodes(out, root)
    return out


def _get_nodes(out, node):
    nid = id(node)
    if nid in out:
        return
    out[nid] = node

    for (_, child) in node.children:
        _get_nodes(out, child)


## TEST STUFF


class TestNodes(object):
    """

            a-->9-->b--4-->e
            |       ^
            2       |
            |       1
            v       |
            c---3---d
    """
    def __init__(self):
        self.a = Node()
        self.b = Node()
        self.c = Node()
        self.d = Node()
        self.e = Node()

        self.a.children = [
            (9, self.b),
            (2, self.c),
        ]

        self.b.children = [
            (4, self.e),
        ]

        self.c.children = [
            (3, self.d),
        ]

        self.d.children = [
            (1, self.b),
        ]

        self.all_nodes = [
            self.a,
            self.b,
            self.c,
            self.d,
            self.e
        ]
        self.node_map = {id(n): n for n in self.all_nodes}


def test_get_nodes():
    nodes = TestNodes()
    assert get_nodes(nodes.a) == nodes.node_map


def test_shortest_path():
    nodes = TestNodes()
    visited = shortest_path(nodes.a)

    assert nodes.a.value == 0
    assert nodes.c.value == 2
    assert nodes.d.value == 5
    assert nodes.b.value == 6
    assert nodes.e.value == 10
