Fisher yates:
```
for i from 0 to n−2 do
     j ← random integer such that i ≤ j < n
     exchange a[i] and a[j]
```

Or in python:
```
for i in xrange(0, len(array) - 1):
    j = random.randint(i, len(array))
    swap(array, i, j)
```

If you only wanted the first k elements, you can imagine
halting the process after that many elements.

In the case where you don't want to modify the array
you can create random *indexes* into an array of `len=n`
by using a hash table instead.

```
# Contains previously swapped elements at the random
# index. This guarantees no replacement
swapped = {}
out = []
for i in xrange(0, n - 1):
    j = random.randint(i, n)

    let val = match swapped.get(j) {
        // j was already used so it contains a previous `i`
        Some(v) => v,
        // j is not yet in swapped
        None => j,
    };

    swapped[j] = match swapped.get(i) {
        // "normal" case, `i` wasn't used so insert it as available in case
        // a future random value selects `j`
        None => i,

        // only happens when j == i
        Some(v) => v,
    };
    out.append(val)

```

Try again
```
let mut swapped = HashSet::with_capacity(n);
let mut out = Vec::with_capacity(n);
for i in 0 .. (n - 1) {
    let j = rand::randint(i .. n);

    // What we are doing here:
    // if `j` was already selected, we get the `i` that
    // was skipped at that time.
    //
    // If `j` was not selected we use it.
    //
    // In both cases and store `i` in case `j` is selected in the future.

    let v = match swapped.entry(j) {
        Entry::Occupied(e) => {
            // `j` has already been used. We will get the value that was
            // cached there (a previous `i`) and insert i as the new available
            // value in case `j` is selected in the future.
            e.insert(i)
        },
        Entry::Vacant(e) => {
            // No value yet exists at j, store i as an "available" value for
            // future iterations and return j
            e.insert(i);
            j
        }
    };
    out.push(v)
}



```


# Reference Python Impl
```
import random

def sample(n, lower, upper):
    result = []
    # pool of (generated: ?) values
    pool = {}
    for _ in xrange(n):
        # generate a random integer within the range
        j = random.randint(lower, upper)

        # ... get the integer if it exists... else return it?
        x = pool.get(j, j)

        # set the integer... what the heck is going on here?
        pool[j] = pool.get(lower, lower)

        # we can no longer select the lower integer?
        lower += 1
        result.append(x)
    return result
```
