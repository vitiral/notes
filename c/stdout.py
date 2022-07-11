
import time
import io
import os
import tty
import sys
import termios
import subprocess
import threading
import select

KILL = False

class KillableReader:
  """A reader that can be killed."""
  def __init__(self, fd):
    self.fd = fd

  def read(self, n, orNewline=False):
    out = bytearray()
    while n:
      if KILL: break
      (rdRdy, wrRdy, xRdy) = select.select([self.fd.fileno()], [], [], 0.2)
      assert not wrRdy
      assert not xRdy
      if rdRdy:
        b = os.read(self.fd.fileno(), n)
        if not b: break # EOF
        out.extend(b)
        n -= len(b)
      if orNewline and b'\n' in out: break
    return out

def readPassLoop(inp, toOut):
  """Reads from input and passes to output."""
  writeBuf = bytearray()
  eof = False

  while True:
    if KILL: return
    (rdRdy, wrRdy, xRdy) = select.select([inp.fileno()], [toOut.fileno()], [], 0.2)
    if rdRdy:
      b = os.read(inp.fileno(), 256)
      if b: writeBuf.extend(b)
      else: eof = True

    if wrRdy and writeBuf:
      written = os.write(toOut.fileno(), writeBuf)
      toOut.flush()
      os.fsync(toOut.fileno())
      print("Wrote input:", writeBuf[:written])
      writeBuf = writeBuf[written:]

    if eof and not writeBuf:
      return

def readFromC(procOut):
  while True:
    b = procOut.read(512, orNewline=True)
    if b: print(f"[py] procOut (len={len(b)}):\n" + b.decode('utf-8'))
    else: return # EOF

p = subprocess.Popen(
    args=['./a.out'],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=sys.stderr,
)

procOut = KillableReader(p.stdout)
tInp = threading.Thread(target=readPassLoop, args=(sys.stdin, p.stdin))
tOut = threading.Thread(target=readFromC, args=(procOut,))

try:
  tInp.start()
  tOut.start()
  while True: time.sleep(10)
finally:
  KILL = True
  tInp.join()
  tOut.join()
