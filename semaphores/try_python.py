from __future__ import print_function
import time
from threading import Thread, Semaphore

s = Semaphore(1)

def doit():
    a = s.acquire()
    print("did it and got:", a)

th1 = Thread(target=doit)
th1.start()

th2 = Thread(target=doit)
th2.start()

th3 = Thread(target=doit)
th3.start()

assert not th1.is_alive()
assert th2.is_alive()
assert th3.is_alive()
assert s._value == 0, "god I hate this, should be -2"

s.release()
sval = s._value
assert sval == 1, "The value is increased to 1 MOMENTARILY"
start = time.time()
while sum([th2.is_alive(), th3.is_alive()]) > 1:
    assert time.time() - start < 0.5
    time.sleep(0.1)

assert s._value == 0, "bizare... when an aquire is awoken, THEN _value is decremented"
print("after release")
print("th1:", th1.is_alive())
print("th2:", th2.is_alive())
print("th3:", th3.is_alive())

s.release()
assert s._value == 1
while sum([th2.is_alive(), th3.is_alive()]) > 0:
    assert time.time() - start < 0.5
    time.sleep(0.1)

assert s._value == 0
