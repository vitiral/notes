"""Implement a read/write lock using only semaphores.
"""
from threading import Semaphore, Thread, Condition
import time

class RWLockRPreferred(object):
    """A readers writer lock where the reader is preferrerd"""

    def __init__(self):
        self._read_lock = Semaphore(1)
        self._write_lock = Semaphore(1)
        self.readers = 0

    def read_lock(self):
        self._read_lock.acquire()
        if self.readers == 0:
            # if we are the first reader, don't allow writers.
            self._write_lock.acquire()
        self.readers += 1
        self._read_lock.release()

    def read_unlock(self):
        self._read_lock.acquire()
        self.readers -= 1
        if self.readers == 0:
            # if we were the last reader, allow writers.
            self._write_lock.release()
        self._read_lock.release()

    def write_lock(self):
        self._write_lock.acquire()

    def write_unlock(self):
        self._write_lock.release()


class RWLockWPreferred(object):
    """A readers writers lock where the writer is preferred.

    Some notes on the condition variable:
    - It is basically a `Lock/Mutex` object with additional `wait` and `release` methods.
    - The extra methods can _only be called after `acquire`_.
    - `wait` releases the lock until `release*` is called by another thread.

    Basically it avoids having to use a mutex-var to unlock/check-var/lock --
    the OS handles when you unlock which is much more efficient.

    """

    def __init__(self):
        self._write_lock = Semaphore(1)
        self._condition = Condition()
        self.readers = 0
        self.writers_waiting = 0

    def read_lock(self):
        self._condition.acquire()

        while self.writers_waiting:
            self._condition.wait()

        if self.readers == 0:
            self._write_lock.acquire()
        self.readers += 1
        self._condition.release()

    def read_unlock(self):
        self._condition.acquire()
        self.readers -= 1
        if self.readers == 0:
            self._write_lock.release()
        self._condition.release()

    def write_lock(self):
        self._condition.acquire()
        self.writers_waiting += 1
        self._condition.release()

        self._write_lock.acquire()

        self._condition.acquire()
        self.writers_waiting -= 1
        if self.writers_waiting == 0:
            self._condition.notify_all()
        self._condition.release()

    def write_unlock(self):
        self._write_lock.release()


def test_read_preferred_basic():
    """Multiple readers + 1 writer.

    - kick off the readers, they always get the same value.
    - kick off the writer
    - assert writer does not complete
    - let readers complete + assert
    - kick off new readers
    - let writer complete + assert
    - let readers complete + assert.

    Note: this takes advantage of the fact that the GIL guarantees
    threadsafe append operations.
    """

    values = [0]
    read_values = []
    complete_readers = []
    complete_extra_reader = []
    complete_writers = [True]
    lock = RWLockRPreferred()

    def read_value(signal):
        lock.read_lock()
        while not signal:
            time.sleep(0.1)
        read_values.append(values[0])
        lock.read_unlock()

    def write_value(v = 1):
        lock.write_lock()
        while not complete_writers:
            time.sleep(0.1)
        values[0] = v
        lock.write_unlock()

    def start_readers():
        readers = [Thread(target=read_value, args=(complete_readers,)) for _ in range(3)]
        for r in readers:
            r.start()
        return readers

    readers = start_readers()
    extra_reader = Thread(target=read_value, args=(complete_extra_reader,))
    writer = Thread(target=write_value)

    # everythign should be started except extra
    writer.start()

    # assert everythign is still alive
    time.sleep(0.3)
    for r in readers:
        assert r.is_alive()
    assert writer.is_alive()

    # start extra reader and sleep
    extra_reader.start()
    time.sleep(0.3)

    # complete the current readers.
    complete_readers.append(True)
    time.sleep(0.3)

    # assert the values are as expected and writer is alive.
    assert read_values == [0] * 3
    assert writer.is_alive()
    assert extra_reader.is_alive()

    readers = start_readers()
    complete_extra_reader.append(True)
    time.sleep(0.3)

    assert read_values == [0] * 7
    assert values == [1]


def test_write_preferred_basic():
    """Multiple readers + 1 writer."""

    values = [0]
    read_values = []
    complete_readers = []
    complete_extra_reader = [True]
    complete_writers = []
    lock = RWLockWPreferred()

    def read_value(signal):
        lock.read_lock()
        while not signal:
            time.sleep(0.1)
        read_values.append(values[0])
        lock.read_unlock()

    def write_value(v = 1):
        lock.write_lock()
        while not complete_writers:
            time.sleep(0.1)
        values[0] = v
        lock.write_unlock()

    def start_readers():
        readers = [Thread(target=read_value, args=(complete_readers,)) for _ in range(3)]
        for r in readers:
            r.start()
        return readers

    readers = start_readers()
    extra_reader = Thread(target=read_value, args=(complete_extra_reader,))
    writer = Thread(target=write_value)

    # everythign should be started except extra
    writer.start()

    # assert everythign is still alive
    time.sleep(0.3)
    extra_reader.start()

    for r in readers:
        assert r.is_alive()
    assert writer.is_alive()
    assert extra_reader.is_alive()

    # complete the readers who were started before the writer
    complete_readers.append(True)

    # assert the values are as expected
    time.sleep(0.3)
    assert writer.is_alive()
    assert read_values == [0] * 3
    assert extra_reader.is_alive()
    assert values == [0]

    # finish the writer
    complete_writers.append(True)
    time.sleep(0.3)
    assert not writer.is_alive()
    assert values == [1]
    assert read_values == [0] * 3 + [1]
