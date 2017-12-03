# Little Book Of Semaphores

- synchronoization: the relationship between events
- in general, the programmer has no control over the synchronization between threads
  and especially between processes.

Semaphore
- When you create a semaphore you can initialize it to any integer value, but afterwards
  you are only allowed to increment (signal) and decrement (wait) it. You cannot read
  the current value.
- When a thread decrements the semaphore, if the *results is negative* then it
  blocks itself and cannot continue until another thread increments the semaphore.
- When a thread increments the semaphore, if there are other threads waiting,
  one of them gets unblocked. **NOTE: the semaphore does not have to be positive
  for another thread to get unblocked. It ONLY HAS TO BE INCREMENTED**.

## Rendevous
Ensure that a1 < b2 and b1 < b2 using only semaphores.

```
statement a1            statement b1
statement a2            statement b2
```

```

            # semaphore to allow a to continue
            allow_a_cont = Semaphore(0)
            # semaphore to allow b to continue
            allow_b_cont = Semaphore(0)

statement a1            statement b1
allow_b_cont.signal()   allow_a_cont.signal()
allow_a_cont.wait()     allow_b_cont.wait()
statement a2            statement b2
```

## Mutex

Puzzle1:

```

mutex = Semaphore(1)

mutex.wait()        |   mutex.wait()
count = count + 1   |   count = count + 1
mutex.signal()      |   mutex.signal()

```

Note: this can be made a multiplex by initializing the Semaphore to N instead of 1


## Barrier
create a solution that requires n threads to reach the rendevous before any (all)
can continue.

What I want to do is:
```
barrier = Semaphore(-n)

spawn(thread_i): # spawn the thread at this index
    barrier.signal()
    CRITICAL
```

However, signaling does NOT block...

A "brute force" solution is that each thread has their own mutex, and
we free all of them before continuing.

```
barrier = [Semaphore(0) for _ in range(n)]

spawn(thread_i):
    # unblock everyone else
    [s.signal() for (i, s) in enumerate(barrier) if i != thread_i]
    barrier[thread_i].wait()
    CRITICAL
```

After hint:
```
n = <num threads>
count = 0
count_mutex = Semaphore(1)
barrier = Semaphore(0)

spawn(thread_i):
    # mark that we have arrived
    count_mutex.wait()
    count += 1
    if count == n:
        for _ in range(n):
            barrier.signal()
    count_mutex.signal()

    barrier.wait()
```

Given solution, using the fact that you only need to INCREMENT the semaphore
to unblock a single tread -- the value does NOT have to be positive!!!
```
n = <num threads>
count = 0
count_mutex = Semaphore(1)
barrier = Semaphore(0)

spawn(thread_i):
    # mark that we have arrived
    count_mutex.wait()
        count += 1
        # unblock one thread if count is complete
        if count == n:
            barrier.signal()
    count_mutex.signal()

    // classic turnstyle, allows one thread to pass at a time
    // and can be locked to bar ALL threads
    barrier.wait()
    barrier.signal()
```

## Reusable Barrier
Puzzle: Rewrite the barrier solution so that after all the th reads have passed
through, the turnstile is locked again.

Actually my answer after the hint would work fine, but so would the other one.
You just need to make *sure* you are only using `count` inside the mutex.

```
n = <num threads>
count = 0
count_mutex = Semaphore(1)
turnstyle1 = Semaphore(0)  # initially closed
turnstyle2 = Semaphore(1)  # initially open


spawn(thread_i):
    count_mutex.wait()
    count += 1
    if count == n:
        # lock the second turnstyle before allowing any
        # threads through. Because count == n here
        # we KNOW only one thread can do this
        turnstyle2.wait()
        turnstyle1.signal()
    count_mutex.signal()

    turnstyle1.wait()
    turnstyle1.signal()

    # CRITICAL

    count_mutex.wait()
    count -= 1
    if count == 0:
        turnstyle1.wait()  # clear the extra increment
        turnstyle2.signal()
    count_mutex.signal()

    turnstyle2.wait()
    turnstyle2.signal()
```

## Queue
A queue (wtf is a queue) can be written using semaphores.
Typically:
- initial value of semaphore is 0
- it is not possible to signal unless there is a thread waiting.
- the value is *never positive*


Imagine that threads represent ballroom dance rs and that two kinds of dancers,
leaders and followers, wait in two queues b efore entering the dance floor.
When a leader arrives, it checks to see if there is a follower waiting.  If so,
they can both proceed. Otherwise it waits.  Similarly, when a follower arrives,
it checks for a leader and either proceeds or waits, accordingly.  Puzzle:
write code for leaders and followers that enforces t hese constraints.

```
leaderQ = Semaphore(0)
leaderMutex = Semaphore(1)
followerQ = Semaphore(0)
followerMutex = Semaphore(1)

spawn(leader):
    leaderMutex.wait()
        followerQ.signal()
        leaderQ.wait()
        dance()
    leaderMutex.signal()

spawn(follower):
    followerMutex.wait()
        leaderQ.signal()
        followerQ.wait()
        dance()
    followerMutex.signal()
```

I prefer my solution since it is simpler, but his solution uses only one
mutex and saves a call to `signal` on a mutex.

```
leaders = 0
followers = 0
leaderQ = Semaphore(0)
followerQ = Semaphore(0)
mutex = Semaphore(1)

spawn(leader):
    mutex.wait()
    if followers > 0:
        followers -= 1
        followerQ.signal()
    else:
        leaders += 1
        mutex.signal()
        leaderQ.wait()

    dance()

    leaders -= 1
    mutex.signal()

spawn(follower):
    mutex.wait()
    if leaders > 0:
        leaders -= 1
        leaderQ.signal()
    else:
        followers += 1
        mutex.signal()
        followerQ.wait()

    dance()
```


## 4.1 Producer Consumer
Add concurrency primities here

```
mutex = Semaphore(1)
items_available = Semaphore(0)
items_full = Semaphore(bufferSize)

spawn(producer):
    local event = waitForEvent()
    items_full.wait()
    with mutex:
        buffer.add(event)
    items_available.signal()


spawn(consumer):
    items_available.wait()
    with mutex:
        event = buffer.get()
    items_full.signal()
    event.process()
```


## 4.2 Readers-Writers Problem

