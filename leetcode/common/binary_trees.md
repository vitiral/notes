
# Binary Tree
A "tree" data structure where each node has at most two children

# Binary Heap:
A binary tree with ONE of
- max-heap: each node is greater than both children
- min-heap: each node is less than both children

# Binary Search Tree
A binary tree with the properties
- the left subtree contains only nodes with keys less than the node's key
- the right subtree contains only nodes with keys greater than the node's key
- the left and right subtree must also be binary search trees
- there must be no duplicates

traversals:
- in-order: named because items are output "in order". traverses [left, self, right]
- pre-order: named because self is output first. traverses [self, left, right]
- post-order: named because self is output last. traverses [left, right, self]

inserts:
- inserts are always added as a leaf. Just traverse the tree comparing to the
  new key and insert when no node exists.

There are some interesting properties:

Day-Stout-Warren algorithm of balancing:
- right/left rotating is legal
```

     5
    / \
   2   7
      / \
     8   9

# rotating A left moves A down-left and pulls ALL right
# nodes for the ride, disconnecting D. D is then connected
# to A (actually several connection/disconnections take place)

     7
    / \
   5   9
  / \
 2   8

# Now if C is rotated right, it results in the first diagram.
```
- you can "flatten" a BST by just rotating right on all nodes
- the algorithm to balance it is then to rotate every ODD
  nodes (except start at root) until you end up with a balanced tree.

```
2
 \
  5
   \
    7
     \
      8
       \
        9

# rotate root first

   5
  / \
 2   7
      \
       8
        \
         9

```

# TODO: understand rotating better
hmm... that's not right... I need to understand rotating better
