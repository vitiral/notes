import sys
import math
import pprint


def next_int(iter_lines):
    return int(next(iter_lines))

def solve_testcases(solve_testcase):
    iter_lines = iter(sys.stdin.readlines())
    num_testcases = next_int(iter_lines)
    for _ in range(num_testcases):
        print(solve_testcase(iter_lines))

# Dijskra's shortest path algorithm
# https://practice.geeksforgeeks.org/problems/shortest-path-from-1-to-n/0


from collections import defaultdict
import heapq

class Graph:
    def __init__(self):
        # dictionary of nodes to their edges
        self._edges = defaultdict(list)

        # distances in the form (a,b): distance
        # all distances are ordered
        self._distances = {}

    @classmethod
    def from_num_nodes(cls, num_nodes):
        graph = cls()
        for node in range(1, num_nodes + 1):
            j = node + 1
            if j <= num_nodes:
                graph.add_edge(node, j)
            j = node * 3
            if j <= num_nodes:
                graph.add_edge(node, j)
        return graph

    def nodes(self):
        return self._edges.values()

    def distance(self, a, b):
        return self._distances[self._convert_distance(a, b)]

    def edges(self, a):
        return self._edges[a]

    def add_edge(self, a, b, distance=1):
        """Set edge from a -> b with a distance."""
        self._edges[a].append(b)
        # self._edges[b].append(a)
        self._distances[self._convert_distance(a, b)] = distance

    @staticmethod
    def _convert_distance(a, b):
        if b < a:
            return (b, a)
        return (a, b)


def dijskras_shortest_path(graph, start, dest):
    visited = defaultdict(lambda: math.inf)
    unvisited = [(0, start)]
    # heapq.heapify(unvisited)

    while unvisited:
        # while there are unvisited nodes pick up each node and calculate
        # the distance to all its children
        distance, node = heapq.heappop(unvisited)
        if distance > visited[node]:
            continue
        assert distance is not math.inf

        visited[node] = distance

        for child in graph.edges(node):
            child_distance = distance + graph.distance(node, child)
            if child_distance < visited[child]:
                visited[child] = child_distance
                heapq.heappush(unvisited, (child_distance, child))

    return visited[dest]


def solve_shortest_path():

    def solve_testcase(iter_lines):
        num_nodes = next_int(iter_lines)
        graph = Graph.from_num_nodes(num_nodes)
        return dijskras_shortest_path(graph, 1, num_nodes)

    solve_testcases(solve_testcase)

# solve_shortest_path()
# N = 887
# graph = Graph.from_num_nodes(N)
# print("Edges:")
# pprint.pprint(graph._edges)
# print("Distances:")
# pprint.pprint(graph._distances)
#
# print("Result:")
# print(dijskras_shortest_path(graph, 1, N))
