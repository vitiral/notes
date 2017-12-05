from __future__ import print_function
import time
import random
from threading import Semaphore, Thread

from semaphore_types import Lightswitch, Barrier

def left(i):
    return i

def right(total, i):
    return (i + 1) % total

class GroupTable(object):
    """The table around which philosophers sit.

    - To eat, each philosopher requires TWO forks. To get these
      forks, they must ALL call ``get_forks()``
    - When each philospher is done eating, they must call ``put_forks()``

    This architecture scales to any number of seats at the table without
    requiring additional semaphores. It does this by separating the
    philosophers into two groups: evens and odds. When evens are eating,
    odds have to wait. When odds are eating, evens have to wait (fork=0
    can have resource contention if ``seats`` is odd).

    This has the negative effect that each group has to wait till **every**
    philosopher of the other group is finished before **any** member of that
    group can eat, even if some of them *could* have eaten before then.

    So, if `think` or `eat` can take a very varied time then this is not a good
    solution.  However, if they are fairly uniform, then this guarantees the
    greatest degree of parallelism possible with the fewest number of
    semaphores (as seats gets larger).
    """

    def __init__(self, seats):
        self.seats = seats
        self.barrier = Barrier(seats)
        self.eating = Semaphore(1)          # lock that one group is eating
        self.evensEating = Lightswitch()
        self.oddsEating = Lightswitch()

        if seats % 2 == 0:
            # there is never resource contention
            self.shared_fork = None
        else:
            # mutex for the zeroth fork
            self.shared_fork = Semaphore(1)

    def must_share(self, i):
        """Return whether this seat must share a fork.

        This only happens when there is an odd number of seats as there can
        be resource contenion for the i=0 fork (0th seat's left, n-1 seat's
        right)

        For example, with seats=5 seat=0's left fork and seat=4's right fork
        are the same fork!

        For other cases, resource contention is guaranteed to not exist
        by the "eating" semaphore and the fact that only evens/odds group
        are allowed to eat at once.
        """
        return self.seats % 2 != 0 and (i == 0 or i == self.seats - 1)

    def get_forks(self, i):
        # wait for all the philosophers to "sit down"
        self.barrier.wait()

        if i % 2 == 0:
            self.evensEating.lock(self.eating)
        else:
            self.oddsEating.lock(self.eating)

        if self.must_share(i):
            self.shared_fork.acquire()

    def put_forks(self, i):
        if i % 2 == 0:
            self.evensEating.unlock(self.eating)
        else:
            self.oddsEating.unlock(self.eating)

        if self.must_share(i):
            self.shared_fork.release()

class BasicTable(object):
    """Note: doens't prevent deadlock."""
    def __init__(self, seats):
        self.seats = seats
        self.forks = [Semaphore(1) for _ in range(seats)]

    def get_forks(self, i):
        self.forks[right(self.seats, i)].acquire()
        self.forks[left(i)].acquire()

    def put_forks(self, i):
        self.forks[right(self.seats, i)].release()
        self.forks[left(i)].release()


class FootmanTable(BasicTable):
    def __init__(self, seats):
        assert seats > 0
        super(FootmanTable, self).__init__(seats)
        self.footman = Semaphore(seats - 1)

    def get_forks(self, i):
        self.footman.acquire()
        super(FootmanTable, self).get_forks(i)

    def put_forks(self, i):
        super(FootmanTable, self).put_forks(i)
        self.footman.release()


class LeftyTable(BasicTable):
    """Table where i=0 is a lefty."""
    def get_forks(self, i):
        if i == 0:
            self.forks[left(i)].acquire()
            self.forks[right(self.seats, i)].acquire()
        else:
            self.forks[right(self.seats, i)].acquire()
            self.forks[left(i)].acquire()


class GroupLefties(BasicTable):
    def get_forks(self, i):
        if i % 2:
            # evens are lefties
            self.forks[left(i)].acquire()
            self.forks[right(self.seats, i)].acquire()
        else:
            self.forks[right(self.seats, i)].acquire()
            self.forks[left(i)].acquire()


def philosopher(index, table):
    think = lambda: time.sleep(random.random() * 0.1)
    eat = lambda: time.sleep(random.random() * 0.2)

    for n in range(10):
        # print("starting philosopher {} loop {}".format(index, n))
        think()
        table.get_forks(index)
        eat()
        table.put_forks(index)

    # print("philosopher {} finished!".format(index))


def runit(num, table_class):
    table = table_class(num)
    threads = [Thread(target=philosopher, args=(i, table)) for i in range(num)]
    for th in threads:
        th.start()

    start = time.time()
    while sum(th.is_alive() for th in threads) != 0:
        time.sleep(0.1)
        assert time.time() - start < 60

    return time.time() - start

if __name__ == '__main__':
    for num in [5, 10, 50]:
        lefty_time = runit(num, LeftyTable)
        footman_time = runit(num, FootmanTable)
        group_time = runit(num, FootmanTable)
        group_lefty_time = runit(num, GroupLefties)

        print("""## NUM={}
Lefty  : {}
Footman: {}
Group  : {}
LeftyGr: {}
""".format(
        num,
        lefty_time,
        footman_time,
        group_time,
        group_lefty_time,
    ))
