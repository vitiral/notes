# Summary

**General Strategy**
- Write down question with all requirements
- Do trivial example by hand + graphed out, reducing scope if needed
- Analyze problem and brainstorm possible data structres / algorithms
- Divide and conquer when possible

**Finding X in string/array**
- sliding window: move a window along the data
- growing midpoint: grow a midpoint at different points in the data
- data structure: HashMap/Set are very commonly useful

**Conversion Questions**
- General strategy (simple cases manually, reducing scope, etc)
- Do alignment work up front, it's always worth it

**Binary Search Trees**
- Validating: "narrow a window" of min/max values, or use inorder traversal
- See how tree rotation can solve the problem
- Self balancing trees: find depths and use left/right to determine how to rotate
- Balanced definition: the heights of two child subtrees differ by at most 1

**Data Structures**
- array
- que/stack/deque
- tree (Binary Tree, Binary Heap)
- hashmap / hashset
- directed/undirected graph (don't know much about)

**Sorting Algorithms**
- O(n^2)     / O(1): quicksort > insertion sort > selection sort > bubble sort
- O(n log n) / O(1): heapsort
- O(n log n) / O(n): timsort > mergesort
- integer case: counting sort, radix sort bucket sort can be as low as O(n+k)
- bucket sorting: if you know the number of possible values is small (i.e.
  1000 employees, plz sort by age) you can easily put each value in a bucket
  (i.e. hash-table) and then sort the buckets
- enourmous-bit-field: if you want to _remove duplicates **and** sort_ then you
  can use an enourmous bitfield to do both in `O(N)`!!
  - create a bitfield of all possible integers. This takes `2^N / 8` bytes, where
    `N` is the size of the integer in bytes.
  - when you encounter an integer simply flip it's relevant bit to 1
  - scan through the array, outputing any `1` as it's corresponding integer.
    Therefore this only takes two passes.

## General Stratey for questions
- Always start off by *writing down* the question and then branching it
  with more questions
  - Get all the requirements. Are all inputs allowed, or only
    positive/negative/rational/etc?
  - Make sure every term is well defined.
- Do a trivial example by hand to make sure you understand the problem
    - Reduce scope if necessary
    - Graph it out in text or do several solutions in table form to look for
      patterns
    - Do each step of what is *wanted* manually, poiting out where things are
      "just working"
    - Find boundary conditions
- Take another look at the problem, what strategies/datastructures/etc are the
  most likely to solve it?
- Discuss the posibilities (with yourself and interviewer), let them know how
  your thought process works and where you are unsure.
- Dive in: try to solve the problem "manually" using the method you came up
  with.


## Strategies
In addition to the below data structures and algorithms, there are some
basic strategies for solving problems that have come up:
- In general
    - Break the problem down into smaller parts/functions
    - If having problems, reduce the scope and solve it manually
      recording what the solution for each line would look like.
- For "finding X in string/vec/etc" there are three main strategies:
    - "sliding window" O(n): see if the the solution can be found by moving
      a window along the data, looking for the solution
    - "growing midpoint" O(n^2): do a brute-force ish method of starting at
      all the "midpoints" and growing windows around them, looking for
      the solution
    - Try to use a data structure or otherwise munge the data
    - If none of those, see if it is solvable (at all) through brute force.
- For conversion/number questions
    - Always do simple cases manually first
    - Reduce the problem space if necessary. Instead of "excel columns" allow
      only `A` and `B` and compare with binary. Instead of "linked list of numbers"
      do a "linked list of binary values".
    - The first step is often to get data on the values and then get them to
      "align". Don't rely on recursion to do your alignment for you, have it
      as an explicit step and grow it
- For binary trees, a common strategy for numeric analysis is to "narrow a
  window" of minimum and maximum values as the tree is traversed. A Binary seach
  tree will:
    - have all left nodes < parent node < their parent node < etc...
    - have all right nodes > parent node > their parent node > etc...
    - this strategy can be applied recursively. As you keep going right the max
      is infinite but the min is the parent node. As soon as you cut left
      the max is permanantly decreased, etc


## Data Structures
- array: contiguous memory with a set size. inserting/deleting can take up to
  O(n), appending takes O(1), locating index takes O(1), searching O(n)
- que: first-in first-out buffer (like the line at a supermarket)
- stack: first-in last-out buffer, like a pile of trays
- deque: stack + queue. Example: ring buffer
- tree: data structure with a root node and branches from it. Last nodes
  are called "leaves"
  - binary tree: a tree where each node can have up to two branches.
  - complete tree: all levels are filled except the last
  - depth-first traversals: recursive "visiting of nodes."
    - inorder (left, self, right), preorder (self, left, right),
      postorder (left, right, self)
    - inorder traversal of BST will result in a sorted output
    - inorder + (pre | post) will result in the tree structure being
      described completely.
    - preorder + duplicating nodes can make a complete duplicate of BTree
    - postorder + deleting nodes can delete the entire tree.
  - breadth-first traversals:
    - pop from que and do (self, left, right), putting on the queue for each
      child. You can use level+=1 in each loop to keep track of level.
    - breadth-first is especially useful for "infinite" trees... although
      the above method wouldn't work well would it!
      ... actually it seems to be the only way?
  - in a binary tree, node N can have following relatives:
    - U: the uncle of the node (grandparent's other child)
    - G: the grandparent (parent of the parent)
    - B: the brother of the node (parent's other child)
    - P: the parent of the node
    - N: the node itself
    - L: the left child of the node
    - R: the right child of the node

```
     "left"         "right"

        G              G
       / \            / \
      P   U          U   P
     / \                / \
    N   B              B   N
   / \                    / \
  L   R                  L   R
     / \
    rL  rR

 ROT RIGHT          ROT LEFT

      G                G
     / \              / \
    N   U            U   N
   / \                  / \
  L   P                P   R
     / \              / \
    R   B            B   L
```
  - flattening of trees can be performed with rotation, especially for BST's
  - right rotation (of tree on the left) of node N means:
    - R becomes P's left-child
    - P becomes N's right child (B stays P's right child)
    - N becomes G's left-child
  - left rotation (of tree on the left) of node N means ... you can't do a left
    rotation because N is the left node of P
  - left rotation (of tree on the right) of node N means:
    - L becomes P's right-child
    - P becomes N's left-child (B stays P's left-child)
    - N becomes G's right-child (it moves up)
- binary heap: binary tree where both branches are less than their parent
  node (max-heap) or vice versa (min-heap)
  - finding the max (in a max-heap) is O(1). Removing it is O(log n)
  - inserting a value is O(log n)
  - can be represented in a single contiguous array.
    `node=k, left=2*k+1, right=2*k+2, parent=(k - 1)//2`
- binary search tree: binary tree where all nodes in the left sub-tree
  are less than any given node and all nodes in the right sub-tree are
  greater.
  - insertions are O(log n), just traverse the tree and insert where it is
    valid
  - rotations are a critical operation, and are used for balancing a tree
    as well as in self-balancing (red-black) trees.


## Sorting algorithms:
- bubble sort: basically the worst. n^2 performance. Only advantage is it can
  easily support best case O(n) performance
- selection sort: you "select" the minimum value by scanning the entire n+1
  array for every n and then swapping it with n. This has very little use
  except as something that is "better than bubble sort".
- insertion sort: natural way to sort playing cards, also n^2 perforamnce. It
  is also a stable sort with O(1) memory requirements. "Insertion" is a
  *really really* bad name, as it does not do *any insertions* (gawd).
  It simply grows a sorted array one a time, each new element it swaps left
  until they are greater than the value to their left. Should be called shift
  sort...
  - O(n^2), stable and inplace
  - More efficient in practice than most other simple quadratic (i.e., O(n^2))
    algorithms such as selection sort or bubble sort
  - Adaptive, i.e., efficient for data sets that are already substantially
    sorted: the time complexity is O(nk) when each element in the input is no
    more than k places away from its sorted position
  - Online/async: can sort a list as it receives it
  - Typically the fastest sorting method for small arrays.
    - used internally in quicksort (and Timsort/MergeRunSort) to sort the
      sub-arrays when the arrays are small (typically smaller than 10-45 elements)
  - Can be used directly on (doubly) linked lists by simply starting with an
    empty list and taking values off of the input list one at a time
    and inserting/shifting them to the correct place.
- Mergesort: very simple really.
    - O(n log n) performance but O(n) memory usage (theoretically, harder to
      implement in practice)
    - Divide and conqure: selects the middle and calls mergesort on both
      sides which keeps doing that.
    - When mergesort returns, each side is assumed sorted and they are
      "merged" by moving them one at a time into a new array.
- Quicksort:
    - chooses a piviot to divide and conquer, worst case happens when after
      pivioting, the list is not divided equally(ish)
    - simple implementation:
        - piviot is last item, start at beginning and swap and increment
          ref-index for every value < piviot
            - sanity check: if all values are < piviot, they will all have been
              swapped with themselves
            - sanity check: the ref-index will EITHER == count OR equal
              a value >= piviot.
        - finally, swap the ref-index with the piviot-index and return piviot
        - it is now known that all values left of piviot are less than it
          and vice-versa
- Heapsort
    - unstable, inplace, O(n log n) for all cases
    - heapify the array and then pop values from it until the array is sorted.

## Basic Probability Theory:
- Poison distribution: `P(k events in t) = e^-l * ( l^k ) / (k!)
  - `l == lambda == average == rate * time [rt]`
  - discrete probability distribution
  - expresses the probability of events within a fixed interval of space and time.
  - events occur with a known constant rate and independently of their last event


## General Algorithms
### Greedy
A greedy algorithm is one that chooses the most optimal solution _locally_ in the hope
that it leads to the most optimal solution globally.

It may not always get the optimal solution, but it often at least approximates it.

## General Strategy
- Writing a regex engine:
  - create function to do match
  - create functions to do most basic things (`match_one`)
  - recursively try _every_ possibility
  - for non `^` (start), do `search` which does `match` on every item.
  - `*` and `?` then just become allowing for literally every possibility

