"""PG-81: Chapter 18 Threads and Locks


In order for a deadlock to occur you must have ALL of the four Coffman
conditions must be met:
 1. Mutual Exclusion: at least one resource must be held in non-shareable mode,
    otherwise the process would not be prevented from using the resource.
 2. Hold and wait: a process is currely holding at least one resource and
    requesting additional resources held by other processes.
 3. No preemption: a resource can be released only voluntarily by the process
    holding it.
 4. Circular wait: each process must be waiting on a resource which is being
    held by another process, which in turn is waiting for the first process.

"""

################################################################################
# 18.1: What’s the difference between a thread and a process?
"""
It depends on the terminology, but generally:
- a thread is a "child" of the main process. If the main process dies, the
  thread dies. It often shares the memory space of other threads in the
  process.
- A process is it's own OS unit containing it's own PID, memory, etc.
"""

################################################################################
# 18.2: How can you measure the time spent in a context switch?
"""
First of all, a context switch is when you give up process time to other threads.
It happens especially often when making syscalls.

To measure the time you should use a monotomically increasing counter which is
tied to your threads CPU frequency. Simply record the value before the switch
and after the switch and take their difference to determine the amount of
time spent in the switch.

# Second Answer knowing what context switch is

A "context switch" is the time it takes to switch from one process to another.
It involves swapping out the registers to use the cached ones from another
process.

Okay, so now to measure the time. Let's assume we have process P1 and it spawns
process P2 and we want to know how long that took.

```
starting_time = monoclock()
process.start()
db.insert(process.pid, STARTING_TIME, starting_time)
```

In the second process, it should record the clock time as well:
```
started_time = monoclock()
db.insert(self.pid, STARTED_TIME, started_time)
```
"""

################################################################################
# 18.3
# Implement a singleton design pattern as a template such that, for any given
# class Foo, you can call Singleton::instance() and get a pointer to an
# instance of a singleton of type Foo.
#
# Assume the existence of a class Lock which has acquire() and release()
# methods. How could you make your implementation thread safe and exception
# safe?


class Singleton(object):
    def __init__(self):
        self.foo = None
        self.lock = Lock()

    # def instance(self):
    #     if self.foo is None:
    #         return Foo()
    #     else:
    #         return self.foo

    def __enter__(self):
        self.lock.acquire()
        if self.foo is None:
            self.foo = Foo()
        return self.foo

    def __exit__(self, *args):
        self.lock.release()


# Actual answer:
# Foo itself didn't need to be threadsafe. Whoops!

class Singleton(object):
    def __init__(self):
        self.foo = None
        self.lock = Lock()

    def instance(self):
        if self.foo is None:
            self.lock.acquire()
            if self.foo is None:
                self.foo = Foo()
            self.lock.release()
        return self.foo


################################################################################
# 18.4
# Design a class which provides a lock only if there are _no possible deadlocks_.

"""
Notes:

Wow, what a requirement... kind of crazy.

Well, what things can lead to a deadlock?
- Mutual exclusion: a resource must be held in a non-shareable mode.  So we
  could make everything readonly or Cow, which would eliminate deadlock.
- Hold and wait: a process must hold _no other resources_ to request this
  resource hard to guarantee using a class.
- No preemption: we could simply kill the other thread that holds our resource!
  Haha!
- Circular wait: processes are depending on eachother for resources in a circle.
  Dining philosophers.

The solution: kill a process if it holds a resource for more than X time.
"""

################################################################################
# 18.5
# Suppose we have the following code:
#
#     class Foo {
#     public:
#           A(.....); /* If A is called, a new thread will be created and
#                      * the corresponding function will be executed. */
#           B(.....); /* same as above */
#           C(.....); /* same as above */
#     }
#     Foo f;
#     f.A(.....);
#     f.B(.....);
#     f.C(.....);
#
# i) Can you design a mechanism to make sure that B is executed after A, and C
#    is executed after B?
# iii) Suppose we have the following code to use `class Foo`. We do not know how the
#      threads will be scheduled in the OS
#
#  Foo f;
#  f.A(.....); f.B(.....); f.C(.....);
#  f.A(.....); f.B(.....); f.C(.....);
#
# Can you design a mechanism to make sure that all the methods will be executed in
# sequence?

"""
Notes:
i) We want to synchronize A, B and C such that `A -> B -> C`.
   This is a standard gating problem. Unfortunately I'm a bit rusty on the
   theory. What we want is a "gate" that does not allow B to be executed until
   A has been executed.

All you _need_ for this is semaphores. A semaphore is an object that:
- Starts at some value. This value cannot be changed except by the processes below
  and cannot be read.
- Can be decremented by any thread. If decremented and is less than 0, it will
  block.
- Can be incremented by any thread. If it is incremented it will always unblock
  some thread that was blocked (regardless of the semaphore's value).

Globals:
- sem_a = Semaphore(1)
- sem_b = Semaphore(0)
- sem_c = Semaphore(0)

A impl:
- sem_a.decrement()
- work_A()
- sem_b.increment()

B impl:
- sem_b.decrement() # wait
- work_B()
- sem_c.increment()

C impl:
- sem_c.decrement() # wait
- work_C()
- sem_a.increment()
"""

################################################################################
# 18.6
# You are given a class with synchronized method A, and a normal method C
#
# If you have two threads in one instance of a program, can they call A at the
# same time? Can they call A and C at the same time?
"""
I'm not super familiar with java syncronized methods, but I believe it basically
puts a lock around the method:

class MyClass(object):
    def mymethod(self):
        with self._mymethod_lock:
            self._mymethod()

    def _mymethod(self):
        '''The user-defined method'''

So the answer is a _bit_ complicated. They could _attempt_ to call A at the same
time, but the function's code would only execute in a single thread.

The could definitely call both A and C at the same time.
"""
