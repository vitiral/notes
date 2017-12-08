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

Syncronization constraints:
1. Any number of readers can be in the critical section simultaneously.
2. Writers must have exclusive access to the critical section.


At a quick glance it looks like we can do this with a simple protected count
of each. How do we wait though?

This seems like a barrier problem:
- while we are reading, readers can use the section at-will
- while we are writing, only one writer can use the section

```
reading = false
writing = false
num_readers = 0
num_writers = 0

readers_turnstyle = Semaphore(0)
writer_exclusive = Semaphore(0)

mutex = Semaphore(1)

spawn(reader):
    with mutex:
        num_readers += 1
        if !writing && !reading:
            reading = true
            readers_turnstyle.signal()

    readers_turnstyle.wait()
    readers_turnstyle.signal()

    CRITICAL

    with mutex:
        num_readers -= 1
        if num_readers == 0:
            reading = false
            readers_turnstyle.wait()  # remove the extra increment
            if num_writers > 0:
                writing = true
                writer_exclusive.signal()

spawn(writer):
    with mutex:
        num_writers += 1
        if !writing && !reading:
            writing = true
            writer_exclusive.signal()

    # only one writer can be signaled at a time
    writer_exclusive.wait()

    CRITICAL

    with mutex:
        num_writers -= 1
        if num_writers > 0:
            writer_exclusive.signal()
        else:
            writing = false
            if num_readers > 0:
                reading = true
                reading_turnstyle.signal()
```

After using the initialized variables in **hint**:
```
readers = 0                     # how many readers are in the room
readers_mutex = Semaphore (1)   # protects readers
# roomEmpty is
# - 1 if no readers in critical section
# - 0 otherwise (how is this possible???)
#
# So, wait means "wait for room to be empty", signal means "room is empty"
roomEmpty = Semaphore (1)

spawn(reader):
    with readers_mutex:
        readers += 1
        if readers == 1:
            # first in locks
            # note that the readers_mutex prevents other readers from accessing
            roomEmpty.wait()

    CRITICAL

    with readers_mutex:
        readers -= 1
        if readers == 0:
            roomEmpty.signal()

spawn(writer):
    # Extremely simple... note that writer *does not use the mutex*
    roomEmpty.wait()
    CRITICAL
    roomEmpty.signal()
```

This pattern is called **lightswitch**, i.e. the first person into the room
turns on the light, the last one out turns it off.

```
class Lightswitch:
    def __init__(self):
        self.count = 0
        self.mutex = Semaphore(1)

    def lock(self, semaphore):
        with self.mutex:
            self.count += 1
            if self.count == 1:
                semaphore.wait()

    def unlock(self, semaphore):
        with self.mutex:
            self.count -= 1
            if self.count == 0:
                semaphore.signal()
```


And the new code:
```
roomEmpty = Semaphore(1)
switch = Lightswitch()

spawn(writer):
    roomEmpty.wait()
    CRITICAL
    roomEmpty.signal()

spawn(reader):
    switch.lock(roomEmpty)
    CRITICAL
    switch.unlock(roomEmpty)
```


### Starvation
The previous solution can easily lead to starvation, where a writer
never gets to enter the room.

To fix this, we should not allow new readers if there is a writer waiting.

This is a first stab, although I feel like this will introduce a deadlock.
```
write_mutex = Semaphore(1)
writers = 0
read_mutex = Semaphore(1)
readers = 0
roomEmpty = Semaphore(1)

writerPriority = Semaphore(1)

spawn(writer):
    with write_mutex:
        writers += 1
    writerPriority.wait()
    roomEmpty.wait()
    CRITICAL
    with write_mutex:
        writers -= 1
    roomEmpty.signal()
    writerPriority.signal()

spawn(reader):
    with read_mutex:
        readers += 1
        if readers == 1:
            roomEmpty.wait()
        with write_mutex:
            if writers > 0:
                writerPriority.wait()

    CRITICAL

    with read_mutex:
        readers -= 1
        if readers == 0:
            roomEmpty.signal()

```

With the hint things are becomming more clear: use a turnstyle for the readers
*that the writers can lock*.

```
readSwitch = Lightswitch()
turnstyle = Semaphore(1)    # a turnstyle for readers and a mutex for writers.
roomEmpty = Semaphore(1)

spawn(writer):
    turnstyle.wait()    # block readers
    roomEmpty.wait()
    turnstyle.signal()  # allow readers through again
    CRITICAL
    roomEmpty.signal()

spawn(reader):
    turnstyle.wait()
    turnstyle.signal()
    switch.lock(roomEmpty)
    CRITICAL
    switch.unlock(roomEmpty)

```

## 4.4 Dining Philosophers

Some analysis:
- deadlock is possible if all threads run at once, i.e. all threads get their
  right fork
- If we only allow n-1 philosophers to run at once, we can guarantee there is no
  deadlock since at least one philosopher will be able to continue.
- On that note, we could use a group-switch, where one group is "even" and the
  other group is "odd"

```
spawn(philosopher)
    while True:
        think()
        get_forks()
        eat()
        put_forks()

# we need to write get_forks and put_forks
forks = [Semaphore(1) for _ in range(5)]

otherAvilable = Semaphore(1)
evensEating = Switch()
oddsEating = Switch()
barrier = Barrier(5)

def left(i):
    """Get the i'th philosopher's left fork"""
    return i

def right(i):
    """Get the i'th philosopher's right fork"""
    return (i + 1) % 5

def get_forks(i):
    barrier.wait()
    if i % 2 == 0:
        evensEating.lock(otherAvailable)
    else:
        oddsEating.lock(otherAvailable)
    # we still need the forks for 1 and 5
    fork[right(i)].wait()
    fork[left(i)].wait()

def put_forks(i):
    if i % 2:
        evensEating.unlock(otherAvailable)
    else:
        oddsEating.unlock(otherAvailable)
    fork[right(i)].signal()
    fork[left(i)].signal()
```

We can improve this even further -- we know that the only fork that can be
potentially

Another solution is to limit the number of philosophers to 4, but you COULD
have only one philosopher eating (2 are guaranteed to eat in this solution).


## Cigarette Smokers Problem

Agent
```
agentSem = Semaphore (1)
tobacco = Semaphore (0)
paper = Semaphore (0)
match = Semaphore (0)
```

Example (need) Tobaco Smoker that doesn't work
```
match.wait()
paper.wait()
```

### First try at solution after hint

```
isTobacco = isPaper = isMatch = False
has_tobacco = Semaphore(0)
has_paper = Semaphore(0)
has_match = Semaphore(0)

spawn(detect_tobacco):
    tobacco.wait()
    isTobacco = True
    tobacco.signal()

spawn(detect_paper):
    paper.wait()
    isPaper = True
    paper.signal()

spawn(detect_match):
    match.wait()
    isMatch = True
    match.signal()

spawn(scheduler):
    while isMatch + isPaper + isTobacco < 2:
        sleep()

    if isTobacco && isPaper:
        has_match.signal()
    elif isTobacco && isMatch:
        has_paper.signal()
    elif isPaper && isMatch:
        has_tobacco.signal()
    else:
        assert False

spawn(paper_smoker):
    has_paper.wait()
    match.wait()
    tobacco.wait()
```



Their solution is better
```
numTobacco = numPaper = numMatch = 0
has_tobacco = Semaphore(0)
has_paper = Semaphore(0)
has_match = Semaphore(0)
mutex = Semaphore(1)

spawn(paperListener):
    paper.wait()
    mutex.wait()
    if numTobaco > 0:
        numTobaco -= 1  # we use one
        has_match.signal()
    elif isMatch:
        numMatch -= 1  # we use one
        has_tobacco.signal()
    else:
        numPaper += 1
    mutex.signal()

spawn(paperSmoker):
    has_paper.wait()
    makeCigarette()
    agent.signal()
    smoke()
```

## Dining Savages Problem

- Savages cannot invoke getServingFromPot if the pot is empty.
- The cook can invoke putServingsInPot only if the pot is empty

```
# Basics
spawn(savage):
    while True:
        getServingFromPot()
        eat()

spawn(cook):
    while True:
        putServingsInPot(M)
```

First stab at solution
```
mutex = Semaphore(1)
servingsInPot = Semaphore(0)
servingsLeft = 0
potEmpty = Semaphore(1)

spawn(cook):
    while True:
        potEmpty.wait()
        mutex.wait()
            putServingsInPot(N)
            servingsLeft += N
            servingsInPot.signal(N)
        mutex.signal()

spawn(savage):
    while True:
        mutex.wait()
            if servingsLeft == 0:
                # found an empty pot, signal the cook
                potEmpty.signal()
            # take one anyway: servingsLeft has to be able to be negative
            servingsLeft -= 1
        mutex.signal()

        servingsInPot.wait()
        getServingFromPot()
        eat()



def getServingsFromPot():

```

## Barbershop Problem
- waiting room with `n` chairs
- barber room with barber chair, who is only one who can do work.
- if no customers -> barber goes to sleep
- if customer enters and all `n` chairs filled, they leave -- else they sit in
  a chair
- if barber is asleep, a customer awakes the barber

Simplify:
- customers call `getHairCut` or `leave` (or `balk`)
- barber thread invokes `cutHair`
    - when `cutHair` is invoked there should be exactly one thread invoking
      `getHairCut`


Notes:
- I think this is solved by a basic queue
- The barber should sleep until a customer "sits down"

```

mutex = Semaphore(1)
waiting = Queue()
customers = Semaphore(0)
customerDone = Semaphore(0)
barberDone = Semaphore(0)

spawn(customer):
    mutex.wait()
    if len(waiting) == n:
        mutex.signal()
        return balk()
    else:
        sem = Semaphore(0)
        waiting.push(sem)
        customers.signal()
    mutex.signal()

    sem.wait()
    # barber.wait()
    getHairCut()
    customerDone.signal()
    barberDone.wait()


spawn(barber):
    while True:
        customers.wait()
        mutex.wait()
            customer = waiting.pop()
        mutex.signal()
        customer.signal()
        cutHair()
        barberDone.signal()
        customerDone.wait()
```

## 5.6 Building H20
We have to create a barrier that enforces that a complete water molecule is ready
before proceeding

```

spawn(hydrogen):
    pass

spawn(oxygen):
    pass

```
