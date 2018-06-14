# PG-68: Chapter 12 System Design and Memory Limits

# 12.1
> If you were integrating a feed of end of day stock price information (open,
> high, low, and closing price) for 5,000 companies, how would you do it?  You
> are responsible for the development, rollout and ongoing monitoring and
> maintenance of the feed.
>
> Describe the different methods you considered and why you would recommend
> your approach. The feed is delivered once per trading day in a
> comma-separated format via an FTP site. The feed will be used by 1000 daily
> users in a web application

First of all, a database of some kind to store the data sounds like the right
approach but lets get a few more requirements.

- The feed is currently used by 1000 daily users in a web app -- how much
  are we expecting/hoping to grow? I assume we want this to scale.
- Can the data we are getting be recreated in cases of emergency, aka is
  it publically available, we are just putting it in a convienient format?
  - If it _can't_ be recreated then we should think very seriously about
    disaster recovery.
- The users of this feed... want it once per day. What are the advantages of
  this feed over others? What are the characteristics of this "feed" that
  make it excel? What is the feed used for?

On a basic level, it sounds like our job is to:
- record the price of each stock at:
  - the opening bell
  - the closing bell
  - running average of the max and min

The above could be defined as a single row in a database that we continuously
update throughout the day with new min/max data for each item.

```
CREATE TABLE StockPrice {
    Ticker varchar(32) NOT NULL,
    Name varchar(255) NOT NULL,

    OpenPrice Price,
    ClosePrice Price,
    MaxPrice Price,
    MinPrice Price,

    PRIMARY KEY (ticker)
}
```

Do _not_ use floating point values for `Price`. Even a string that gets
converted into a proper datatype is better.

Now, assuming we are continuously polling the stock price throughout the day
it would _not_ be good to update each row in the table everytime a new price
is gotten. Instead:
- poll all prices for some time N, caching the min/max/start/end in memory
  and on filesystem, etc
- Once every period N, get the last `StockPrice` values and compute the
  new min/max/start/end. Update the table accordingly.


Some thoughts:
- We could use an in-memory database, like reddis, to store the cache.
- We could use a distributed database to improve reliability.
- We could have multiple clients to poll the stock indexes. However, there
  isn't a _ton_ of value in this since if we are 30 seconds behind it is
  probably not that big a deal. So having multiple clients and using a
  heartbeat to ensure only one is polling/updating the DB at a time is probably
  good enough.

Final solution:
- Now at the end of the day/delivery time we simply pull the data from the
  database and serialize it into the csv file which we put on our ftp server.


# 12.2

> How would you design the data structures for a very large social network
> (Facebook, LinkedIn, etc)?
>
> Describe how you would design an algorithm to show the connection, or path,
> between two people (eg, `Me -> Bob -> Susan -> Jason -> You`)

So at a basic level you need the `Person` class.

```
class Person(object):
    def __init__(self, name, birthday, relationship_status, connections, bio):
        pass

    def connect(self, other: Person):
        pass


class Connection(object):
    def __init__(self, person_a, person_b):
        pass
```

At a fundamental level:
- Each person has a 64bit unique id.
- Each person is connected to their connections, which they can store
  as a set of ids.
- Checking if a person connected to another is therefore simply a set lookup.
- Naive shortest path calculations with graphs this large are typically done
  breadth-first, with some maximum limit on the depth that will be looked for.
  This is because connections are quite often not far away (max of 6 rule).
- However, breadth first searches require a lot of memory/cache to store the
  queue to lookup.
- In addition, we need to avoid cycles (this graph has them... lots of them...)
- You can significantly reduce the amount of memory and eliminate cycles
  by conducting two simultanious searches while keeping a hash table of found
  people in both.
  - breadth-first search from Me -> You
  - breadth-first search from You -> Me
  - Keep a set of all visited items in each.
    - If you've already visited someone, then don't revisit them.
    - If the other searcher has already visited someone... you've found
      a connection! You're done
  - uses `O(|V| + |E|)` time (vertexes + edges). `|E|` can be between
    `O(1) .. O(V^2)` depending on how sparse the input graph is.
  - uses `O(|V|)` memory, or memory proportional to the number of vertices (total?).
- You can use memoization/caching/dynamic-programming for a bunch of these.
    - Cache in a DB who is connected to who so you can look it up in O(1) time.



# 12.3
> Given an input file with four billion integers, provide an
> algorithm to generate an integer which is not contained in the file.
>
> Assume you have 1 GB of memory
>
> FOLLOW UP
> What if you have only 10 MB of memory?

The first thing that comes to mind is to sort the file using mergesort,
which was originally designed to be used to sort _tape drives_, so it can
definitely handle sorting on the filesystem with any amount of memory.

Since we have 1GB of memory we can trivially sort 1GB worth of integers at
a time using an inplace sorting method like quicksort or heapsort, and then
mergesort the different pieces.

Once we have a sorted array generating an integer that isn't in the file is
trivial:

```
def return_missing_integers(sorted_file):
    # a cached iterative loading of the integers in the file
    integers = FileIntegers(sorted_file)

    try:
        current_integer = integers.next()
    except IntegersExhausted:
        # There are no integers in the file.
        # return all integers from [0 MAX_INT]
        #
        # Alternatively we may want to raise
        # a different error.
        current_integer = None

    for i in range(0, MAX_INT+1):
        if i == current_integer:
            current_integer = integers.next()
        else:
            yield i

    raise IntegersExhausted()
```

Some thoughts:
- If the algorithm is interrupted it cannot be restarted -- the generated
  integers would be lost.
  - To avoid this we could store generated integers in a separate file.
    when the algorithm is kicked off again it sees the new file and
    merge-sorts it into the sorted file. I could imagine some different
    file names for the file types:
    - `unsorted`: the original file would be named this
    - `sorted`: each sorted file would be named this. The algorithm would
      always merge-sort these together at the beginning and then delete
      the originals.
      - Note: the generated numbers would just go into one of these files.
- The serialization format was not specified. I would recommend a binary
  flatfile using the appropriate number of bytes per integer with some kind
  of metadata header in case we need to change the file at all.

## "Answer"
The book recommends using a bitfield.

- there are 2^32 = 4 billion possible integers
- there is 1GB of memory or 8 billion bits.

You can thus map each integer value to a distinct bit. This is _pretty clever_,
but I like it.

