from __future__ import print_function
import time
import random
from threading import Semaphore, Thread

from semaphore_types import Lightswitch, Barrier



class Table(object):
    """The table around which philosophers sit.

    - To eat, each philosopher requires TWO forks. To get these
      forks, they must ALL call ``get_forks()``
    - When each philospher is done eating, they must call ``put_forks()``

    This architecture scales to any number of seats at the table without
    requiring additional semaphores. It does this by separating the
    philosophers into two groups: evens and odds. When evens are eating,
    odds have to wait. When odds are eating, evens have to wait (fork=0
    can have resource contention).

    This has the negative effect that each group has to wait till *every*
    philosopher of the other group is finished before *any* member can eat.

    Another approach (which WOUlD require a semaphore for each seat) would
    be to allow (for example) the odd group to always eat first, wait
    till they all started and then allow the even group to eat.
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

def philosopher(index, table):
    think = lambda: time.sleep(0.1 + random.random() * 0.1)
    eat = lambda: time.sleep(0.1 + random.random() * 0.2)

    for n in range(10):
        print("starting philosopher {} loop {}".format(index, n))
        think()
        table.get_forks(index)
        eat()
        table.put_forks(index)

    print("philosopher {} finished!".format(index))

if __name__ == '__main__':
    NUM = 99
    table = Table(NUM)
    threads = [Thread(target=philosopher, args=(i, table)) for i in range(NUM)]
    for th in threads:
        th.start()

    start = time.time()
    while sum(th.is_alive() for th in threads) != 0:
        time.sleep(0.1)
        assert time.time() - start < 60
