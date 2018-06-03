# Top Level Stuff

  Common Questions          | Project 1         | Project 2         | Project 3         |
 -------------------------- | ----------------- | ----------------- | ----------------- |
 Challenges                 |                   |                   |                   |
 Mistakes/Failures          |                   |                   |                   |
 Enjoyed                    |                   |                   |                   |
 Leadership                 |                   |                   |                   |
 Conflicts                  |                   |                   |                   |
 What I'd Do Different      |                   |                   |                   |

## Strengths

## Weaknesses

## Technical Projects

## Hardest Bug I've Had to Solve

## Questions For Interviewer

## Strategy
- Listen, especially to details that are abnormal
- Make a medium size (not edge-case) example to help understand the problem.
- Simplify and generalize: tweak a requirement to make it easier to solve then
  try to solve more generally.
- Try to solve the problem "manually" -- what process did I use?
- Get a brute force solution and determine best conceivable runtime.
- Base Case and build: any kind of recursive build-up problem starts small.
  Use the small cases and build on them.

# Big O Notation

- Best Conceivable Run Time: what is the fastest you could _possibly imagine_.
  - Must each element be touched at least once? Then at least `O(n)`
  - What are all the variables in play? Are there multiple variable lengths?
  - Can a hashtable be used? Each operation is `O(1)`
  - Can a binary search-tree/heap be used? If so `O(log n)`
- `log N`: binary search + trees + etc. Happens when each decision halves
  (or more) the space.
- `2^N`: this frequently happens when a function is guaranteed to call itself twice.
   `fn a(x): ... return a(x-1) + a(x+1)`
- **Ammortized time**: if it can be proven that the _average_ time is some value then
  it can use used.


# Linked Lists
- Runner technique:
  - you use two pinters, a "fast" one and a "slow" one to "weave" the linked list.


# Graphs and Trees
- Graph: directed or undirected set of nodes/vertices with edges.
- Tree: A tree is just a graph where all nodes are connected and there are no cycles.
- Complete Binary Tree: every level (except maybe the last) is filled and last is filled
  left -> right.
  - Can be stored in an array with `left=2*n + 1, right=2*n + 2, parent=(n - 1)//2`
- Full Binary Tree: all nodes have zero _or_ two children. Can be full but not complete.
  (or vice versa)
- Perfect: complete & full. Pretty rare.
- Tries: `n-ary` tree where each node is a character and the path down a tree represents
  a word. `*` nodes or a boolean flag mark the end of a word.
- Search types:
  - **depth first**: typically uses recursion. pre-order, in-order and post-order. This is
    often better if we want to visit every node.
  - **breadth first**: best implemented iteratively with a queue. Often best
    for "shortest path" searches.

## AVL Trees
- Each node stores the height of the subtrees rootated at that node.
- Any node can therefore check if the height is balanced.

## Red Black Trees
- every node is either red or black
- the root is always black
- the leaves (null nodes) are black
- red can only have black children (red nodes must have two black children)
- therefore: every path from a node to it's leaves must have the same # of black children

Insertion:
- All inserted nodes are red. If the parent is red then there is a red-red violation.
- if uncle is red, just toggle the colors of P, U and G then check G's parent.
- if U is black:
  - case A: N+P are both left children
    - rotate P right and flip P+G color
  - case B: P is left child, N is right child
    - ... not doing any more...

## Bit Manipulation

### XOR:
- if both are 0 or 1 => 0
- else               => 1

00 => 0
01 => 1
10 => 1
11 => 0

### 2's compliment
Using 4 bits
```
|3| = 0011
-3
    = 2^4 + (2^(4-1) - 3 = 5)
    = 1101
# OR invert then add 1
    = ~3 + 1
    = 1101
```

Masks:
- `&` with `0` will always turn "off"
- `|` with `1` will always turn "on"

# System Design and Scalability
System design is mostly about your ability to get a rough design on the board and think out loud.

- What is the scope of the problem? Get a scope/requirements first.
- Make reasonable assumptions _out loud_.
- What is a _basic_ design? What are its major components?
- What are the key difficulties with this design?
- Redesign with the key issues in mind and loop.

## Key Concepts
- Scaling:
  - Verticle Scaling: increase the memory/cpu/etc of individual nodes.
    Improving the performance characteristics of the program itself.
  - Horizontal Scaling: increase the _number of nodes_ and split up the work.
- Load Balancer: a tool that distrubutes work among nodes to balance their loads.
- Database Denormalization: the one I know the least about. Basically databases don't
  always scale well, especially regarding join operations. NoSQL can theoretically help in some
  situations.
  - Try and avoid extra lookups -- instead possibly store the relevant relationships within
    the relevant tables.
- Database Partitioning (sharding): break up the data across machines by some scheme.
  - Verticle Partitioning: split up the data by application/feature. I.e. one partition
    for tables related to profiles, another for messages, etc.
  - Key-Based (or Hash-Based) Partitioning: Use some part of the data (i.e. ID) to partition
    it. Example: `server_index=key % n`. Says the number of servers is "effectively fixed"
    because adding nodes means you have to repartition.
  - Directory-Based Partitioning: you maintain a lookup-table for where data can be found.
    Makes it easy to add servers but you have to maintain and use the lookup table.
- Caching: pretty clear, keep hot stuff in memory and check memory first.
- Async Processing and Queues: make long running processes asyncronous and don't let the user
  see them. Also, be okay with having _some_ delay in what the user sees, being okay to show
  them a slightly out of date version that renders quickly.
- Networking Metrics:
  - Throughput: the speed of the network in real-world scenarios.
  - Bandwidth: the maximum throughput of the system in ideal conditions.
  - Latency: the time it takes for the transmission of a packet to be received.

### Map Reduce:
Map Reduce is a big one, so I'm focusing on it.

Basics:
- I like to call it `Map-(Combine-Shuffle-Reduce)+`
- The Map step is only executed _once_. It takes some data and emits a bunch
  of `(key, value)` pairs.
- The (Combine-Shuffle-Reduce)` step is recursively executed one or more times.
  - Combine: the `(key, value)` are combined so that the values of like-keys
    are joined into `(key, List[value])`. This step can be customized in some
    implementations.
  - Shuffle: organize the (key, List[value]) pairs and "shuffles" them off to
    workers. This is typically defined by the system.
  - Reduce: take in `(key, List[value])` and process it in _some way_,
    producing a bunch of `(key, value)` pairs to again be recursively
    `(Combine-Shuffle-Reduce)`.

#### Hadoop Example
> - http://ercoppa.github.io/HadoopInternals/AnatomyMapReduceJob.html
> - https://docs.marklogic.com/guide/mapreduce/hadoop

Configuration: the user provides configuration:
- A `.jar` with `map()` and `reduce()` implemented (also arbitrary "combiner"
  which can process the data in some way during the map step, i.e. take the
  local maximum/minum etc)
- `input,output` directories/files
- other parameters

Map Task:
- Typically does one `file` per task, but may split the file up.
- Tries to explit data locality, giving data for the map to the node it resides
  on (or it's rack). The load balancer makes the final call though.
- A single map task: it executes the `map()` function and "spills" the results
  to disk. `shuffle` then package the merged keys into outputs.
  - spilling happens when a buffer is getting full. It literally in-memory
    sorts the buffer and dumps them to the local file (I assume it only sorts
    the buffer up to the "current index" since items will still be being
    written by the map step).

Reduce Task on one node:
- fetches the map results from other nodes to be local.
- sorts them into a single sorted array of `(key, value-list)` pairs.
- execute the reduce phase and saves the result (i.e. to HDFS).

The job is complete when all there are no duplicate keys and all reduce tasks
have been executed.

## Consideratiosn
- Failures: essentially any part of a system can fail. Plan for them.
- Availability: percentage of the time a system is available.
- Realibility: probability that a system will be available for a given unit of
  time.
- Read vs Write heavy: queing for writing and caching for reads might make
  sense.
- Security issues
