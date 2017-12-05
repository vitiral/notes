import time
from threading import Semaphore, Thread

class Lightswitch(object):
    def __init__(self):
        self.count = 0
        self.mutex = Semaphore(1)

    def lock(self, semaphore):
        self.mutex.acquire()
        try:
            self.count += 1
            if self.count == 1:
                # first one in the room turns on the lights
                semaphore.acquire()
        finally:
            self.mutex.release()

    def unlock(self, semaphore):
        self.mutex.acquire()
        try:
            self.count -= 1
            if self.count == 0:
                # last one out turns lights off
                semaphore.release()
        finally:
            self.mutex.release()


class Barrier(object):
    """Barrier object that synchronizes ``num`` threads at the barrier.
    """
    def __init__(self, num):
        self.num = num
        self.count = 0
        self.mutex = Semaphore(1)
        self.turnstyle1 = Semaphore(0)
        self.turnstyle2 = Semaphore(1)

    def phase1(self):
        self.mutex.acquire()
        try:
            self.count += 1
            if self.count == self.num:
                # all threads have hit the first turnstyle:
                # - lock the second turnstyle
                self.turnstyle2.acquire()

                # - release the first turnstyle
                self.turnstyle1.release()
        finally:
            self.mutex.release()

        self.turnstyle1.acquire()
        self.turnstyle1.release()

    def phase2(self):
        self.mutex.acquire()
        try:
            self.count -= 1
            if self.count == 0:
                # all threads have hit the second turnstyle:
                # - relock the first turnstyle
                self.turnstyle1.acquire()

                # - release the second turnstyle
                self.turnstyle2.release()
        finally:
            self.mutex.release()

        self.turnstyle2.acquire()
        self.turnstyle2.release()

    def wait(self):
        self.phase1()
        self.phase2()


def test_lightswitch():
    lightswitch = Lightswitch()
    sem = Semaphore(1)

    lightswitch.lock(sem)
    assert sem._value == 0
    lightswitch.lock(sem)
    assert sem._value == 0

    lightswitch.unlock(sem)
    assert sem._value == 0
    lightswitch.unlock(sem)
    assert sem._value == 1


def test_barrier():
    start = time.time()
    def assert_time():
        assert time.time() - start < 0.5
        time.sleep(0.05)

    num = 0
    mutex = Semaphore(1)
    barrier = Barrier(3)

    def doit():
        nonlocal num
        barrier.wait()
        mutex.acquire()
        try:
            num += 1
        finally:
            mutex.release()

    def spawnit():
        th = Thread(target=doit)
        th.start()
        return th

    th1 = spawnit()
    th2 = spawnit()

    start = time.time()
    while sum([th1.is_alive(), th2.is_alive()]) != 2:
        assert_time()

    assert barrier.count == 2
    assert barrier.turnstyle1._value == 0
    assert num == 0

    # import pdb; pdb.set_trace()
    # barrier.wait()
    th3 = spawnit()

    while num < 3:
        assert_time()

    assert num == 3

